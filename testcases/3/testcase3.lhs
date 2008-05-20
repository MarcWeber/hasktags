-- list not complete
-- to be found DBM  
-- to be found ifNull
-- to be found result
-- to be found result'
-- to be fonud selectNestedMultiResultSet

|
Module      :  Database.Enumerator
Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
License     :  BSD-style
Maintainer  :  oleg@pobox.com, alistair@abayley.org
Stability   :  experimental
Portability :  non-portable

Abstract database interface, providing a left-fold enumerator
and cursor operations.

There is a stub: "Database.Stub.Enumerator".
This lets you run the test cases without having a working DBMS installation.
This isn't so valuable now, because it's dead easy to install Sqlite,
but it's still there if you want to try it.

Additional reading:

 * <http://pobox.com/~oleg/ftp/Haskell/misc.html#fold-stream>

 * <http://pobox.com/~oleg/ftp/papers/LL3-collections-enumerators.txt>

 * <http://www.eros-os.org/pipermail/e-lang/2004-March/009643.html>

Note that there are a few functions that are exported from each DBMS-specific
implementation which are exposed to the API user, and which are part of
the Takusen API, but are not (necessarily) in this module.
They include:

 * @connect@ (obviously DBMS specific)

 * @prepareQuery, prepareLargeQuery, prepareCommand, sql, sqlbind, prefetch, cmdbind@

These functions will typically have the same names and intentions,
but their specific types and usage may differ between DBMS.


Had better keep the old style, for older versions of GHC.

> {-# OPTIONS -cpp #-}
> {-# OPTIONS -fglasgow-exts #-}
> {-# OPTIONS -fallow-overlapping-instances #-}
> {-# OPTIONS -fallow-undecidable-instances #-}

New style extension declarations.

> {-# LANGUAGE CPP #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE UndecidableInstances #-}


> module Database.Enumerator
>   (
>     -- * Usage
>
>     -- $usage_example
>
>     -- ** Iteratee Functions
>
>     -- $usage_iteratee
>
>     -- ** result and result'
>
>     -- $usage_result
>
>     -- ** Rank-2 types, ($), and the monomorphism restriction
>
>     -- $usage_rank2_types
>
>     -- ** Bind Parameters
>
>     -- $usage_bindparms
>
>     -- ** Multiple (and nested) Result Sets
>
>     -- $usage_multiresultset
>
>     -- * Sessions and Transactions
>       DBM  -- The data constructor is not exported
>     , withSession, withContinuedSession
>     , commit, rollback, beginTransaction
>     , withTransaction
>     , IE.IsolationLevel(..)
>     , execDDL, execDML, inquire
>
>     -- * Exceptions and handlers
>     , DBException(..)
>     , formatDBException, basicDBExceptionReporter
>     , reportRethrow, reportRethrowMsg
>     , catchDB, catchDBError, ignoreDBError, IE.throwDB
>
>     -- * Preparing and Binding
>     , PreparedStmt(..)  -- data constructor not exported
>     , withPreparedStatement
>     , withBoundStatement, IE.bindP
>
>     -- * Iteratees and Cursors
>     , doQuery
>     , IterResult, IterAct
>     , IE.currentRowNum, NextResultSet(..), RefCursor(..)
>     , cursorIsEOF, cursorCurrent, cursorNext
>     , withCursor
>
>     -- * Utilities
>     , ifNull, result, result'
>   ) where

> import Prelude hiding (catch)
> import Data.Dynamic
> import Data.IORef
> import Data.Time
> import Control.Monad.Trans (liftIO)
> import Control.Exception (throw, 
>            dynExceptions, throwDyn, bracket, Exception, finally)
> import qualified Control.Exception (catch)
> import Control.Monad.Fix
> import Control.Monad.Reader
> import Control.Exception.MonadIO
> import qualified Database.InternalEnumerator as IE
> import Database.InternalEnumerator (DBException(..))


-----------------------------------------------------------


-----------------------------------------------------------


| 'IterResult' and 'IterAct' give us some type sugar.
Without them, the types of iteratee functions become
quite unwieldy.

> type IterResult seedType = Either seedType seedType
> type IterAct m seedType = seedType -> m (IterResult seedType)

| Catch 'Database.InteralEnumerator.DBException's thrown in the 'DBM'
monad.

> catchDB :: CaughtMonadIO m => m a -> (DBException -> m a) -> m a
> catchDB action handler = gcatch action $ \e ->
>   maybe (throw e) handler (dynExceptions e >>= fromDynamic)


|This simple handler reports the error to @stdout@ and swallows it
i.e. it doesn't propagate.

> basicDBExceptionReporter :: CaughtMonadIO m => DBException -> m ()
> basicDBExceptionReporter e = liftIO (putStrLn (formatDBException e))

| This handler reports the error and propagates it
(usually to force the program to halt).

> reportRethrow :: CaughtMonadIO m => DBException -> m a
> --reportRethrow e = basicDBExceptionReporter e >> IE.throwDB e
> reportRethrow e = reportRethrowMsg "" e

| Same as reportRethrow, but you can prefix some text to the error
(perhaps to indicate which part of your program raised it).

> reportRethrowMsg :: CaughtMonadIO m => String -> DBException -> m a
> reportRethrowMsg m e = liftIO (putStr m) >> basicDBExceptionReporter e >> IE.throwDB e

| A show for 'Database.InteralEnumerator.DBException's.

> formatDBException :: DBException -> String
> formatDBException (DBError (ssc, sssc) e m) =
>   ssc ++ sssc ++ " " ++ (show e) ++ ": " ++ m
> formatDBException (DBFatal (ssc, sssc) e m) =
>   ssc ++ sssc ++ " " ++ (show e) ++ ": " ++ m
> formatDBException (DBUnexpectedNull r c) =
>   "Unexpected null in row " ++ (show r) ++ ", column " ++ (show c) ++ "."
> formatDBException (DBNoData) = "Fetch: no more data."


|If you want to trap a specific error number, use this.
It passes anything else up.

> catchDBError :: (CaughtMonadIO m) =>
>   Int -> m a -> (DBException -> m a) -> m a
> catchDBError n action handler = catchDB action
>   (\dberror ->
>     case dberror of
>       DBError ss e m | e == n -> handler dberror
>       _ | otherwise -> IE.throwDB dberror
>   )

| Analogous to 'catchDBError', but ignores specific errors instead
(propagates anything else).

> ignoreDBError :: (CaughtMonadIO m) => Int -> m a -> m a
> ignoreDBError n action = catchDBError n action (\e -> return undefined)

--------------------------------------------------------------------
-- ** Session monad
--------------------------------------------------------------------

The DBM data constructor is NOT exported. 

One may think to quantify over sess in |withSession|. We won't need
any mark then, I gather.
The quantification over Session is quite bothersome: need to enumerate
all class constraints for the Session (like IQuery, DBType, etc).

> newtype IE.ISession sess => DBM mark sess a = DBM (ReaderT sess IO a)
#ifndef __HADDOCK__
>   deriving (Functor, Monad, MonadIO, MonadFix, MonadReader sess)
#else
>   -- Haddock can't cope with the "MonadReader sess" instance
>   deriving (Functor, Monad, MonadIO, MonadFix)
#endif
> unDBM (DBM x) = x


> instance IE.ISession si => CaughtMonadIO (DBM mark si) where
>   gcatch a h = DBM ( gcatch (unDBM a) (unDBM . h) )
>   gcatchJust p a h = DBM ( gcatchJust p (unDBM a) (unDBM . h) )

| Typeable constraint is to prevent the leakage of Session and other
marked objects.

> withSession :: (Typeable a, IE.ISession sess) => 
>     IE.ConnectA sess -> (forall mark. DBM mark sess a) -> IO a
> withSession (IE.ConnectA connecta) m = 
>   bracket (connecta) (IE.disconnect) (runReaderT (unDBM m))


| Persistent database connections. 
This issue has been brought up by Shanky Surana. The following design
is inspired by that exchange.

On one hand, implementing persistent connections is easy. One may say we should
have added them long time ago, to match HSQL, HDBC, and similar
database interfaces. Alas, implementing persistent connection
safely is another matter. The simplest design is like the following

 > withContinuedSession :: (Typeable a, IE.ISession sess) => 
 >     IE.ConnectA sess -> (forall mark. DBM mark sess a) -> 
 >     IO (a, IE.ConnectA sess)
 > withContinuedSession (IE.ConnectA connecta) m = do
 >     conn <- connecta
 >     r <- runReaderT (unDBM m) conn
 >     return (r,(return conn))

so that the connection object is returned as the result and can be
used again with withContinuedSession or withSession. The problem is
that nothing prevents us from writing:

 >     (r1,conn) <- withContinuedSession (connect "...") query1
 >     r2        <- withSession conn query2
 >     r3        <- withSession conn query3

That is, we store the suspended connection and then use it twice.
But the first withSession closes the connection. So, the second
withSession gets an invalid session object. Invalid in a sense that
even memory may be deallocated, so there is no telling what happens
next. Also, as we can see, it is difficult to handle errors and
automatically dispose of the connections if the fatal error is
encountered.

All these problems are present in other interfaces...  In the
case of a suspended connection, the problem is how to enforce the
/linear/ access to a variable. It can be enforced, via a
state-changing monad. The implementation below makes
the non-linear use of a suspended connection a run-time checkable
condition. It will be generic and safe - fatal errors close the
connection, an attempt to use a closed connection raises an error, and
we cannot reuse a connection. We have to write:

 >     (r1, conn1) <- withContinuedSession conn  ...
 >     (r2, conn2) <- withContinuedSession conn1 ...
 >     (r3, conn3) <- withContinuedSession conn2 ...

etc. If we reuse a suspended connection or use a closed connection,
we get a run-time (exception). That is of course not very
satisfactory - and yet better than a segmentation fault. 

> withContinuedSession :: (Typeable a, IE.ISession sess) => 
>     IE.ConnectA sess -> (forall mark. DBM mark sess a) 
>     -> IO (a, IE.ConnectA sess)
> withContinuedSession (IE.ConnectA connecta) m = 
>    do conn <- connecta  -- this invalidates connecta
>       r <- runReaderT (unDBM m) conn
>            `Control.Exception.catch` (\e -> IE.disconnect conn >> throw e)
>       -- make a new, one-shot connecta
>       hasbeenused <- newIORef False
>       let connecta = do
>                      fl <- readIORef hasbeenused
>                      when fl $ error "connecta has been re-used"
>                      writeIORef hasbeenused True
>                      return conn
>       return (r,IE.ConnectA connecta)



> beginTransaction ::
>   (MonadReader s (ReaderT s IO), IE.ISession s) =>
>   IE.IsolationLevel -> DBM mark s ()
> beginTransaction il = DBM (ask >>= \s -> lift $ IE.beginTransaction s il)
> commit :: IE.ISession s => DBM mark s ()
> commit = DBM( ask >>= lift . IE.commit )
> rollback :: IE.ISession s => DBM mark s ()
> rollback = DBM( ask >>= lift . IE.rollback )


> executeCommand :: IE.Command stmt s => stmt -> DBM mark s Int
> executeCommand stmt = DBM( ask >>= \s -> lift $ IE.executeCommand s stmt )

| DDL operations don't manipulate data, so we return no information.
If there is a problem, an exception will be raised.

> execDDL :: IE.Command stmt s => stmt -> DBM mark s ()
> execDDL stmt = executeCommand stmt >> return ()

| Returns the number of rows affected.

> execDML :: IE.Command stmt s => stmt -> DBM mark s Int
> execDML = executeCommand

| Allows arbitrary actions to be run the DBM monad.
the back-end developer must supply instances of EnvInquiry,
which is hidden away in "Database.InternalEnumerator".
An example of this is 'Database.Sqlite.Enumerator.LastInsertRowid'.

> inquire :: IE.EnvInquiry key s result => key -> DBM mark s result
> inquire key = DBM( ask >>= \s -> lift $ IE.inquire key s )

--------------------------------------------------------------------
-- ** Statements; Prepared statements
--------------------------------------------------------------------

> newtype PreparedStmt mark stmt = PreparedStmt stmt

> executePreparation :: IE.IPrepared stmt sess bstmt bo =>
>        IE.PreparationA sess stmt -> DBM mark sess (PreparedStmt mark stmt)
> executePreparation (IE.PreparationA action) =
>     DBM( ask >>= \sess -> lift $ action sess >>= return . PreparedStmt)

> data NextResultSet mark stmt = NextResultSet (PreparedStmt mark stmt)
> data RefCursor a = RefCursor a


The exception handling in withPreparedStatement looks awkward,
but there's a good reason...

Suppose there's some sort of error when we call destroyStmt.
The exception handler also must call destroyStmt (because the exception
might have also come from the invocation of action), but calling destroyStmt
might also raise a new exception (for example, a different error is raised
if you re-try a failed CLOSE-cursor, because the transaction is aborted).
So we wrap this call with a catch, and ensure that the original exception
is preserved and re-raised.

| Prepare a statement and run a DBM action over it.
This gives us the ability to re-use a statement,
for example by passing different bind values for each execution.

The Typeable constraint is to prevent the leakage of marked things.
The type of bound statements should not be exported (and should not be
in Typeable) so the bound statement can't leak either.

> withPreparedStatement ::
>  (Typeable a, IE.IPrepared stmt sess bstmt bo)
>  => IE.PreparationA sess stmt
>  -- ^ preparation action to create prepared statement;
>  --   this action is usually created by @prepareQuery\/Command@
>  -> (PreparedStmt mark stmt -> DBM mark sess a)
>  -- ^ DBM action that takes a prepared statement
>  -> DBM mark sess a
> withPreparedStatement pa action = do
>   ps <- executePreparation pa
>   gcatch ( do
>        v <- action ps
>        destroyStmt ps
>        return v
>     ) (\e -> gcatch (destroyStmt ps >> throw e) (\_ -> throw e))


Not exported.

> destroyStmt :: (IE.ISession sess, IE.IPrepared stmt sess bstmt bo)
>   => PreparedStmt mark stmt -> DBM mark sess ()
> destroyStmt (PreparedStmt stmt) = DBM( ask >>= \s -> lift $ IE.destroyStmt s stmt )



| Applies a prepared statement to bind variables to get a bound statement,
which is passed to the provided action.
Note that by the time it is passed to the action, the query or command
has usually been executed.
A bound statement would normally be an instance of
'Database.InternalEnumerator.Statement', so it can be passed to
'Database.Enumerator.doQuery'
in order to process the result-set, and also an instance of
'Database.InternalEnumerator.Command', so that we can write
re-usable DML statements (inserts, updates, deletes).

The Typeable constraint is to prevent the leakage of marked things.
The type of bound statements should not be exported (and should not be
in Typeable) so the bound statement can't leak either.

> withBoundStatement ::
>   (Typeable a, IE.IPrepared stmt s bstmt bo)
>   => PreparedStmt mark stmt
>   -- ^ prepared statement created by withPreparedStatement
>   -> [IE.BindA s stmt bo]
>   -- ^ bind values
>   -> (bstmt -> DBM mark s a)
>   -- ^ action to run over bound statement
>   -> DBM mark s a
> withBoundStatement (PreparedStmt stmt) ba f =
>   DBM ( ask >>= \s -> 
>     lift $ IE.bindRun s stmt ba (\b -> runReaderT (unDBM (f b)) s))


--------------------------------------------------------------------
-- ** Buffers and QueryIteratee
--------------------------------------------------------------------


|The class QueryIteratee is not for the end user. It provides the
interface between the low- and the middle-layers of Takusen. The
middle-layer - enumerator - is database-independent then.

> class MonadIO m => QueryIteratee m q i seed b |
>     i -> m, i -> seed, q -> b where
>   iterApply ::    q -> [b] -> seed -> i -> m (IterResult seed)
>   allocBuffers :: q -> i -> IE.Position -> m [b]

|This instance of the class is the terminating case
i.e. where the iteratee function has one argument left.
The argument is applied, and the result returned.

> instance (IE.DBType a q b, MonadIO m) =>
>   QueryIteratee m q (a -> seed -> m (IterResult seed)) seed b where
>   iterApply q [buf] seed fn  = do
>     v <- liftIO $ IE.fetchCol q buf
>     fn v seed
>   allocBuffers q _ n = liftIO $ 
>         sequence [IE.allocBufferFor (undefined::a) q n]


|This instance of the class implements the starting and continuation cases.

> instance (QueryIteratee m q i' seed b, IE.DBType a q b)
>     => QueryIteratee m q (a -> i') seed b where
>   iterApply q (buffer:moreBuffers) seed fn = do
>     v <- liftIO $ IE.fetchCol q buffer
>     iterApply q moreBuffers seed (fn v)
>   allocBuffers q fn n = do
>     buffer <- liftIO $ IE.allocBufferFor (undefined::a) q n
>     moreBuffers <- allocBuffers q (undefined::i') (n+1)
>     return (buffer:moreBuffers)



--------------------------------------------------------------------
-- ** A Query monad and cursors
--------------------------------------------------------------------


> type CollEnumerator i m s = i -> s -> m s
> type Self           i m s = i -> s -> m s
> type CFoldLeft      i m s = Self i m s -> CollEnumerator i m s

|A DBCursor is an IORef-mutable-pair @(a, Maybe f)@, where @a@ is the result-set so far,
and @f@ is an IO action that fetches and returns the next row (when applied to True),
or closes the cursor (when applied to False).
If @Maybe@ f is @Nothing@, then the result-set has been exhausted
(or the iteratee function terminated early),
and the cursor has already been closed.

> newtype DBCursor mark ms a =
>     DBCursor (IORef (a, Maybe (Bool-> ms (DBCursor mark ms a))))


| The left-fold interface.

> doQuery :: (IE.Statement stmt sess q,
>             QueryIteratee (DBM mark sess) q i seed b,
>             IE.IQuery q sess b) =>
>      stmt  -- ^ query
>   -> i     -- ^ iteratee function
>   -> seed  -- ^ seed value
>   -> DBM mark sess seed
> doQuery stmt iteratee seed = do
>   (lFoldLeft, finalizer) <- doQueryMaker stmt iteratee
>   gcatch (fix lFoldLeft iteratee seed)
>       (\e -> do
>         finalizer
>         liftIO (throw e)
>       )


An auxiliary function, not seen by the user.

> doQueryMaker stmt iteratee = do
>     sess <- ask
>     query <- liftIO $ IE.makeQuery sess stmt
>     buffers <- allocBuffers query iteratee 1
>     let
>       finaliser =
>            liftIO (mapM_ (IE.freeBuffer query) buffers)
>         >> liftIO (IE.destroyQuery query)
>       hFoldLeft self iteratee initialSeed = do
>         let
>           handle seed True = iterApply query buffers seed iteratee
>             >>= handleIter
>           handle seed False = (finaliser) >> return seed
>           handleIter (Right seed) = self iteratee seed
>           handleIter (Left seed) = (finaliser) >> return seed
>         liftIO (IE.fetchOneRow query) >>= handle initialSeed
>     return (hFoldLeft, finaliser)


Another auxiliary function, also not seen by the user.

> openCursor stmt iteratee seed = do
>     ref <- liftIO (newIORef (seed,Nothing))
>     (lFoldLeft, finalizer) <- doQueryMaker stmt iteratee
>     let update v = liftIO $ modifyIORef ref (\ (_, f) -> (v, f))
>     let
>       close finalseed = do
>         liftIO$ modifyIORef ref (\_ -> (finalseed, Nothing))
>         finalizer
>         return (DBCursor ref)
>     let
>       k' fni seed' = 
>         let
>           k fni' seed'' = do
>             let k'' flag = if flag then k' fni' seed'' else close seed''
>             liftIO$ modifyIORef ref (\_->(seed'', Just k''))
>             return seed''
>         in do
>           liftIO$ modifyIORef ref (\_ -> (seed', Nothing))
>           do {lFoldLeft k fni seed' >>= update}
>           return $ DBCursor ref
>     k' iteratee seed



|cursorIsEOF's return value tells you if there are any more rows or not.
If you call 'cursorNext' when there are no more rows,
a 'DBNoData' exception is thrown.
Cursors are automatically closed and freed when:

 * the iteratee returns @Left a@

 * the query result-set is exhausted.

To make life easier, we've created a 'withCursor' function,
which will clean up if an error (exception) occurs,
or the code exits early.
You can nest them to get interleaving, if you desire:

 >  withCursor query1 iter1 [] $ \c1 -> do
 >    withCursor query2 iter2 [] $ \c2 -> do
 >      r1 <- cursorCurrent c1
 >      r2 <- cursorCurrent c2
 >      ...
 >      return something


Note that the type of the functions below is set up so to perpetuate
the mark.

> cursorIsEOF :: DBCursor mark (DBM mark s) a -> DBM mark s Bool
> cursorIsEOF (DBCursor ref) = do
>   (_, maybeF) <- liftIO $ readIORef ref
>   return $ maybe True (const False) maybeF

|Returns the results fetched so far, processed by iteratee function.

> cursorCurrent :: DBCursor mark (DBM mark s) a -> DBM mark s a
> cursorCurrent (DBCursor ref) = do
>   (v, _) <- liftIO $ readIORef ref
>   return v

|Advance the cursor. Returns the cursor. The return value is usually ignored.

> cursorNext :: DBCursor mark (DBM mark s) a
>     -> DBM mark s (DBCursor mark (DBM mark s) a)
> cursorNext (DBCursor ref) = do
>   (_, maybeF) <- liftIO $ readIORef ref
>   maybe (IE.throwDB DBNoData) ($ True) maybeF


Returns the cursor. The return value is usually ignored.
This function is not available to the end user (i.e. not exported).
The cursor is closed automatically when its region exits. 

> cursorClose c@(DBCursor ref) = do
>   (_, maybeF) <- liftIO $ readIORef ref
>   maybe (return c) ($ False) maybeF


|Ensures cursor resource is properly tidied up in exceptional cases.
Propagates exceptions after closing cursor.
The Typeable constraint is to prevent cursors and other marked values
(like cursor computations) from escaping.

> withCursor ::
>   ( Typeable a, IE.Statement stmt sess q
>   , QueryIteratee (DBM mark sess) q i seed b
>   , IE.IQuery q sess b
>   ) =>
>      stmt  -- ^ query
>   -> i     -- ^ iteratee function
>   -> seed  -- ^ seed value
>   -> (DBCursor mark (DBM mark sess) seed -> DBM mark sess a)  -- ^ action taking cursor parameter
>   -> DBM mark sess a
> withCursor stmt iteratee seed action =
>   gbracket (openCursor stmt iteratee seed) cursorClose action


Although withTransaction has the same structure as a bracket,
we can't use bracket because the resource-release action
(commit or rollback) differs between the success and failure cases.

|Perform an action as a transaction: commit afterwards,
unless there was an exception, in which case rollback.

> withTransaction :: (IE.ISession s) =>
>   IE.IsolationLevel -> DBM mark s a -> DBM mark s a
> 
> withTransaction isolation action = do
>     beginTransaction isolation
>     gcatch ( do
>         v <- action
>         commit
>         return v
>       ) (\e -> rollback >> throw e )


--------------------------------------------------------------------
-- ** Misc.
--------------------------------------------------------------------


|Useful utility function, for SQL weenies.

> ifNull :: Maybe a  -- ^ nullable value
>   -> a  -- ^ value to substitute if first parameter is null i.e. 'Data.Maybe.Nothing'
>   -> a
> ifNull value subst = maybe subst id value



| Another useful utility function.
Use this to return a value from an iteratee function (the one passed to
'Database.Enumerator.doQuery').
Note that you should probably nearly always use the strict version.

> result :: (Monad m) => IterAct m a
> result x = return (Right x)


|A strict version. This is recommended unless you have a specific need for laziness,
as the lazy version will gobble stack and heap.
If you have a large result-set (in the order of 10-100K rows or more),
it is likely to exhaust the standard 1M GHC stack.
Whether or not 'result' eats memory depends on what @x@ does:
if it's a delayed computation then it almost certainly will.
This includes consing elements onto a list,
and arithmetic operations (counting, summing, etc).

> result' :: (Monad m) => IterAct m a
> result' x = return (Right $! x)


That's the code... now for the documentation.


====================================================================
== Usage notes
====================================================================


$usage_example

Let's look at some example code:

 > -- sample code, doesn't necessarily compile
 > module MyDbExample is
 >
 > import Database.Oracle.Enumerator
 > import Database.Enumerator
 > ...
 >
 > query1Iteratee :: (Monad m) => Int -> String -> Double -> IterAct m [(Int, String, Double)]
 > query1Iteratee a b c accum = result' ((a, b, c):accum)
 >
 > -- non-query actions.
 > otherActions session = do
 >   execDDL (sql "create table blah")
 >   execDML (sql "insert into blah ...")
 >   commit
 >   -- Use withTransaction to delimit a transaction.
 >   -- It will commit at the end, or rollback if an error occurs.
 >   withTransaction Serialisable ( do
 >     execDML (sql "update blah ...")
 >     execDML (sql "insert into blah ...")
 >     )
 >
 > main :: IO ()
 > main = do
 >   withSession (connect "user" "password" "server") ( do
 >     -- simple query, returning reversed list of rows.
 >     r <- doQuery (sql "select a, b, c from x") query1Iteratee []
 >     liftIO $ putStrLn $ show r
 >     otherActions session
 >     )

 Notes:

 * connection is made by 'Database.Enumerator.withSession',
   which also disconnects when done i.e. 'Database.Enumerator.withSession'
   delimits the connection.
   You must pass it a connection action, which is back-end specific,
   and created by calling the 'Database.Sqlite.Enumerator.connect'
   function from the relevant back-end.

 * inside the session, the usual transaction delimiter commands are usable
   e.g. 'Database.Enumerator.beginTransaction' 'Database.InternalEnumerator.IsolationLevel',
   'Database.Enumerator.commit', 'Database.Enumerator.rollback', and
   'Database.Enumerator.withTransaction'.
   We also provide 'Database.Enumerator.execDML' and 'Database.Enumerator.execDDL'.

 * non-DML and -DDL commands - i.e. queries - are processed by
   'Database.Enumerator.doQuery' (this is the API for our left-fold).
   See more explanation and examples below in /Iteratee Functions/ and
   /Bind Parameters/ sections.

The first argument to 'Database.Enumerator.doQuery' must be an instance of  
'Database.InternalEnumerator.Statement'.
Each back-end will provide a useful set of @Statement@ instances
and associated constructor functions for them.
For example, currently all back-ends have:

  * for basic, all-text statements (no bind variables, default row-caching)
    which can be used as queries or commands: 

 >      sql "select ..."

  * for a select with bind variables:

 >      sqlbind "select ..." [bindP ..., bindP ...]

  * for a select with bind variables and row caching:

 >      prefetch 100 "select ..." [bindP ..., bindP ...]

  * for a DML command with bind variables:

 >      cmdbind "insert into ..." [bindP ..., bindP ...]

  * for a reusable prepared statement: we have to first create the
    prepared statement, and then bind in a separate step.
    This separation lets us re-use prepared statements:

 >      let stmt = prepareQuery (sql "select ...")
 >      withPreparedStatement stmt $ \pstmt ->
 >        withBoundStatement pstmt [bindP ..., bindP ...] $ \bstmt -> do
 >          result <- doQuery bstmt iter seed
 >          ...

The PostgreSQL backend additionally requires that when preparing statements,
you (1) give a name to the prepared statement,
and (2) specify types for the bind parameters.
The list of bind-types is created by applying the
'Database.PostgreSQL.Enumerator.bindType' function
to dummy values of the appropriate types. e.g.

 > let stmt = prepareQuery "stmtname" (sql "select ...") [bindType "", bindType (0::Int)]
 > withPreparedStatement stmt $ \pstmt -> ...

A longer explanation of prepared statements and
bind variables is in the Bind Parameters section below.


$usage_iteratee

'Database.Enumerator.doQuery' takes an iteratee function, of n arguments.
Argument n is the accumulator (or seed).
For each row that is returned by the query,
the iteratee function is called with the data from that row in
arguments 1 to n-1, and the current accumulated value in the argument n.

The iteratee function returns the next value of the accumulator,
wrapped in an 'Data.Either.Either'.
If the 'Data.Either.Either' value is @Left@, then the query will terminate,
returning the wrapped accumulator\/seed value.
If the value is @Right@, then the query will continue, with the next row
begin fed to the iteratee function, along with the new accumulator\/seed value.

In the example above, @query1Iteratee@ simply conses the new row (as a tuple)
to the front of the accumulator.
The initial seed passed to 'Database.Enumerator.doQuery' was an empty list.
Consing the rows to the front of the list results in a list
with the rows in reverse order.

The types of values that can be used as arguments to the iteratee function
are back-end specific; they must be instances of the class
'Database.InternalEnumerator.DBType'.
Most backends directly support the usual lowest-common-denominator set
supported by most DBMS's: 'Data.Int.Int', 'Data.Char.String',
'Prelude.Double', 'Data.Time.UTCTime'.
('Data.Int.Int64' is often, but not always, supported.)

By directly support we mean there is type-specific marshalling code
implemented.
Indirect support for 'Text.Read.Read'- and 'Text.Show.Show'-able types
is supported by marshalling to and from 'Data.Char.String's.
This is done automatically by the back-end;
there is no need for user-code to perform the marshalling,
as long as instances of 'Text.Read.Read' and 'Text.Show.Show' are defined.

The iteratee function operates in the 'DBM' monad,
so if you want to do IO in it you must use 'Control.Monad.Trans.liftIO'
(e.g. @liftIO $ putStrLn \"boo\"@ ) to lift the IO action into 'DBM'.

The iteratee function is not restricted to just constructing lists.
For example, a simple counter function would ignore its arguments,
and the accumulator would simply be the count e.g.

 > counterIteratee :: (Monad m) => Int -> IterAct m Int
 > counterIteratee _ i = result' $ (1 + i)

The iteratee function that you pass to 'Database.Enumerator.doQuery'
needs type information,
at least for the arguments if not the return type (which is typically
determined by the type of the seed).
The type synonyms 'IterAct' and 'IterResult' give some convenience
in writing type signatures for iteratee functions:

 > type IterResult seedType = Either seedType seedType
 > type IterAct m seedType = seedType -> m (IterResult seedType)

Without them, the type for @counterIteratee@ would be:

 > counterIteratee :: (Monad m) => Int -> Int -> m (Either Int Int)

which doesn't seem so onerous, but for more elaborate seed types
(think large tuples) it certainly helps e.g.

 > iter :: Monad m =>
 >      String -> Double -> CalendarTime -> [(String, Double, CalendarTime)]
 >   -> m (Either [(String, Double, CalendarTime)] [(String, Double, CalendarTime)] )

reduces to (by using 'IterAct' and 'IterResult'):

 > iter :: Monad m =>
 >      String -> Double -> CalendarTime -> IterAct m [(String, Double, CalendarTime)]



$usage_result

The 'result' (lazy) and @result\'@ (strict) functions are another convenient shorthand
for returning values from iteratee functions. The return type from an iteratee is actually
@Either seed seed@, where you return @Right@ if you want processing to continue,
or @Left@ if you want processing to stop before the result-set is exhausted.
The common case is:

 > query1Iteratee a b c accum = return (Right ((a, b, c):accum))

which we can write as

 > query1Iteratee a b c accum = result $ (a, b, c):accum)

We have lazy and strict versions of @result@. The strict version is almost certainly
the one you want to use. If you come across a case where the lazy function is useful,
please tell us about it. The lazy function tends to exhaust the stack for large result-sets,
whereas the strict function does not.
This is due to the accumulation of a large number of unevaluated thunks,
and will happen even for simple arithmetic operations such as counting or summing.

If you use the lazy function and you have stack\/memory problems, do some profiling.
With GHC:

 * ensure the iteratee has its own cost-centre (make it a top-level function)

 * compile with @-prof -auto-all@

 * run with @+RTS -p -hr -RTS@

 * run @hp2ps@ over the resulting @.hp@ file to get a @.ps@ document, and take a look at it.
   Retainer sets are listed on the RHS, and are prefixed with numbers e.g. (13)CAF, (2)SYSTEM.
   At the bottom of the @.prof@ file you'll find the full descriptions of the retainer sets.
   Match the number in parentheses on the @.ps@ graph with a SET in the @.prof@ file;
   the one at the top of the @.ps@ graph is the one using the most memory.

You'll probably find that the lazy iteratee is consuming all of the stack with lazy thunks,
which is why we recommend the strict function.



$usage_rank2_types

In some examples we use the application operator ($) instead of parentheses
(some might argue that this is a sign of developer laziness).
At first glance, ($) and conventional function application via juxtaposition
seem to be interchangeable e.g.

 > liftIO (putStrLn (show x))

 looks equivalent to

 > liftIO $ putStrLn $ show x

But they're not, because Haskell's type system gives us a nice compromise.

In a Hindley-Milner type system (like ML) there is no difference between
($) and function application, because polymorphic functions are not
first-class and cannot be passed to other functions.
At the other end of the scale, ($) and function application in System F
are equivalent, because polymorphic functions can be passed to other
functions. However, type inference in System F is undecidable.

Haskell hits the sweet spot: maintaining full inference,
and permitting rank-2 polymorphism, in exchange for very few
type annotations. Only functions that take polymorphic functions (and
thus are higher-rank) need type signatures. Rank-2 types can't be
inferred. The function ($) is a regular, rank-1 function, and so
it can't take polymorphic functions as arguments and return
polymorphic functions.

Here's an example where ($) fails: 
we supply a simple test program in the README file.
If you change the @withSession@ line to use ($), like so
(and remove the matching end-parenthese):

 >   withSession (connect "sqlite_db") $ do

then you get the error:

 > Main.hs:7:38:
 >     Couldn't match expected type `forall mark. DBM mark Session a'
 >            against inferred type `a1 b'
 >     In the second argument of `($)', namely
 >       ...

Another way of rewriting it is like this, where we separate the
'Database.Enumerator.DBM' action into another function:

 > {-# OPTIONS -fglasgow-exts #-}
 > module Main where
 > import Database.Sqlite.Enumerator
 > import Control.Monad.Trans (liftIO)
 > main = flip catchDB reportRethrow $
 >   withSession (connect "sqlite_db") hello
 >
 > hello = withTransaction RepeatableRead $ do
 >     let iter (s::String) (_::String) = result s
 >     result <- doQuery (sql "select 'Hello world.'") iter ""
 >     liftIO (putStrLn result)

which gives this error:

 > Main.hs:9:2:
 >     Inferred type is less polymorphic than expected
 >       Quantified type variable `mark' is mentioned in the environment:
 >         hello :: DBM mark Session () (bound at Main.hs:15:0)
 >         ...

This is just the monomorphism restriction in action.
Sans a type signature, the function `hello' is monomorphised
(that is, `mark' is replaced with (), per GHC rules).
This is easily fixed by adding this type declaration:

 > hello :: DBM mark Session ()




$usage_bindparms

Support for bind variables varies between DBMS's.

We call 'Database.Enumerator.withPreparedStatement' function to prepare
the statement, and then call 'Database.Enumerator.withBoundStatement'
to provide the bind values and execute the query.
The value returned by 'Database.Enumerator.withBoundStatement'
is an instance of the 'Database.InternalEnumerator.Statement' class,
so it can be passed to 'Database.Enumerator.doQuery' for result-set processing.

When we call 'Database.Enumerator.withPreparedStatement', we must pass
it a \"preparation action\", which is simply an action that returns
the prepared query. The function to create this action varies between backends,
and by convention is called 'Database.PostgreSQL.Enumerator.prepareQuery'.
For DML statements, you must use 'Database.PostgreSQL.Enumerator.prepareCommand',
as the library needs to do something different depending on whether or not the
statement returns a result-set.

For queries with large result-sets, we provide 
'Database.PostgreSQL.Enumerator.prepareLargeQuery',
which takes an extra parameter: the number of rows to prefetch
in a network call to the server.
This aids performance in two ways:
1. you can limit the number of rows that come back to the
client, in order to use less memory, and
2. the client library will cache rows, so that a network call to
the server is not required for every row processed.

With PostgreSQL, we must specify the types of the bind parameters
when the query is prepared, so the 'Database.PostgreSQL.Enumerator.prepareQuery'
function takes a list of 'Database.PostgreSQL.Enumerator.bindType' values.
Also, PostgreSQL requires that prepared statements are named,
although you can use \"\" as the name.

With Sqlite and Oracle, we simply pass the query text to
'Database.PostgreSQL.Sqlite.prepareQuery',
so things are slightly simpler for these backends.

Perhaps an example will explain it better:

 > postgresBindExample = do
 >   let
 >     query = sql "select blah from blahblah where id = ? and code = ?"
 >     iter :: (Monad m) => String -> IterAct m [String]
 >     iter s acc = result $ s:acc
 >     bindVals = [bindP (12345::Int), bindP "CODE123"]
 >     bindTypes = [bindType (0::Int), bindType ""]
 >   withPreparedStatement (prepareQuery "stmt1" query bindTypes) $ \pstmt -> do
 >     withBoundStatement pstmt bindVals $ \bstmt -> do
 >       actual <- doQuery bstmt iter []
 >       liftIO (print actual)

Note that we pass @bstmt@ to 'Database.Enumerator.doQuery';
this is the bound statement object created by
'Database.Enumerator.withBoundStatement'.

The Oracle\/Sqlite example code is almost the same, except for the
call to 'Database.Sqlite.Enumerator.prepareQuery':

 > sqliteBindExample = do
 >   let
 >     query = sql "select blah from blahblah where id = ? and code = ?"
 >     iter :: (Monad m) => String -> IterAct m [String]
 >     iter s acc = result $ s:acc
 >     bindVals = [bindP (12345::Int), bindP "CODE123"]
 >   withPreparedStatement (prepareQuery query) $ \pstmt -> do
 >     withBoundStatement pstmt bindVals $ \bstmt -> do
 >       actual <- doQuery bstmt iter []
 >       liftIO (print actual)

It can be a bit tedious to always use the @withPreparedStatement+withBoundStatement@
combination, so for the case where you don't plan to re-use the query,
we support a short-cut for bundling the query text and parameters.
The next example is valid for PostgreSQL, Sqlite, and Oracle
(the Sqlite implementation provides a dummy 'Database.Sqlite.Enumerator.prefetch'
function to ensure we have a consistent API).
Sqlite has no facility for prefetching - it's an embedded database, so no
network round-trip - so the Sqlite implementation ignores the prefetch count:

 > bindShortcutExample = do
 >   let
 >     iter :: (Monad m) => String -> IterAct m [String]
 >     iter s acc = result $ s:acc
 >     bindVals = [bindP (12345::Int), bindP "CODE123"]
 >     query = prefetch 1000 "select blah from blahblah where id = ? and code = ?" bindVals
 >   actual <- doQuery query iter []
 >   liftIO (print actual)

A caveat of using prefetch with PostgreSQL is that you must be inside a transaction.
This is because the PostgreSQL implementation uses a cursor and \"FETCH FORWARD\"
to implement fetching a block of rows in a single network call,
and PostgreSQL requires that cursors are only used inside transactions.
It can be as simple as wrapping calls to 'Database.Enumerator.doQuery' by
'Database.Enumerator.withTransaction',
or you may prefer to delimit your transactions elsewhere (the API supports
'Database.InternalEnumerator.beginTransaction' and
'Database.InternalEnumerator.commit', if you prefer to use them):

 >   withTransaction RepeatableRead $ do
 >     actual <- doQuery query iter []
 >     liftIO (print actual)

You may have noticed that for 'Data.Int.Int' and 'Prelude.Double' literal
bind values, we have to tell the compiler the type of the literal.
I assume this is due to interaction (which I don't fully understand and therefore
cannot explain in any detail) with the numeric literal defaulting mechanism.
For non-numeric literals the compiler can determine the correct types to use.

If you omit type information for numeric literals, from GHC the error
message looks something like this:

 > Database/PostgreSQL/Test/Enumerator.lhs:194:4:
 >     Overlapping instances for Database.InternalEnumerator.DBBind a
 >                                  Session
 >                                  Database.PostgreSQL.PGEnumerator.PreparedStmt
 >                                  Database.PostgreSQL.PGEnumerator.BindObj
 >       arising from use of `bindP' at Database/PostgreSQL/Test/Enumerator.lhs:194:4-8
 >     Matching instances:
 >       Imported from Database.PostgreSQL.PGEnumerator:
 >     instance (Database.InternalEnumerator.DBBind (Maybe a)
 >                              Session
 >                              Database.PostgreSQL.PGEnumerator.PreparedStmt
 >                              Database.PostgreSQL.PGEnumerator.BindObj) =>
 >          Database.InternalEnumerator.DBBind a
 >                             Session
 >                             Database.PostgreSQL.PGEnumerator.PreparedStmt
 >                             Database.PostgreSQL.PGEnumerator.BindObj
 >       Imported from Database.PostgreSQL.PGEnumerator:
 >     instance Database.InternalEnumerator.DBBind (Maybe Double)
 >                        ....


$usage_multiresultset

Support for returning multiple result sets from a single
statement exists for PostgreSQL and Oracle.
Such functionality does not exist in Sqlite.

The general idea is to invoke a database procedure or function which
returns cursor variables. The variables can be processed by
'Database.Enumerator.doQuery' in one of two styles: linear or nested.

/Linear style:/

If we assume the existence of the following PostgreSQL function,
which is used in the test suite in "Database.PostgreSQL.Test.Enumerator":

 > CREATE OR REPLACE FUNCTION takusenTestFunc() RETURNS SETOF refcursor AS $$
 > DECLARE refc1 refcursor; refc2 refcursor;
 > BEGIN
 >     OPEN refc1 FOR SELECT n*n from t_natural where n < 10 order by 1;
 >     RETURN NEXT refc1;
 >     OPEN refc2 FOR SELECT n, n*n, n*n*n from t_natural where n < 10 order by 1;
 >     RETURN NEXT refc2;
 > END;$$ LANGUAGE plpgsql;

... then this code shows how linear processing of cursors would be done:

 >   withTransaction RepeatableRead $ do
 >   withPreparedStatement (prepareQuery "stmt1" (sql "select * from takusenTestFunc()") []) $ \pstmt -> do
 >   withBoundStatement pstmt [] $ \bstmt -> do
 >     dummy <- doQuery bstmt iterMain []
 >     result1 <- doQuery (NextResultSet pstmt) iterRS1 []
 >     result2 <- doQuery (NextResultSet pstmt) iterRS2 []
 >   where
 >     iterMain :: (Monad m) => (RefCursor String) -> IterAct m [RefCursor String]
 >     iterMain c acc = result (acc ++ [c])
 >     iterRS1 :: (Monad m) => Int -> IterAct m [Int]
 >     iterRS1 i acc = result (acc ++ [i])
 >     iterRS2 :: (Monad m) => Int -> Int -> Int -> IterAct m [(Int, Int, Int)]
 >     iterRS2 i i2 i3 acc = result (acc ++ [(i, i2, i3)])

Notes:

 * the use of a 'Database.Enumerator.RefCursor' 'Data.Char.String'
   type in the iteratee function indicates
   to the backend that it should save each cursor value returned,
   which it does by stuffing them into a list attached to the
   prepared statement object.
   This means that we /must/ use 'Database.Enumerator.withPreparedStatement'
   to create a prepared statement object; the prepared statament oject
   is the container for the cursors returned.

 * in this example we choose to discard the results of the first iteratee.
   This is not necessary, but in this case the only column is a
   'Database.Enumerator.RefCursor', and the values are already saved
   in the prepared statement object.

 * saved cursors are consumed one-at-a-time by calling 'Database.Enumerator.doQuery',
   passing 'Database.Enumerator.NextResultSet' @pstmt@
   (i.e. passing the prepared statement oject wrapped by
   'Database.Enumerator.NextResultSet').
   This simply pulls the next cursor off the list
   - they're processed in the order they were pushed on (FIFO) -
   and processes it with the given iteratee.

 * if you try to process too many cursors i.e. make too many calls
   to 'Database.Enumerator.doQuery' passing 'Database.Enumerator.NextResultSet' @pstmt@,
   then an exception will be thrown.
   OTOH, failing to process returned cursors will not raise errors,
   but the cursors will remain open on the server according to whatever scoping
   rules the server applies.
   For PostgreSQL, this will be until the transaction (or session) ends.

/Nested style:/

The linear style of cursor processing is the only style supported by
MS SQL Server and ODBC (which we do not yet support).
However, PostgreSQL and Oracle also support using nested cursors in queries.

Again for PostgreSQL, assuming we have these functions in the database:

 > CREATE OR REPLACE FUNCTION takusenTestFunc(lim int4) RETURNS refcursor AS $$
 > DECLARE refc refcursor;
 > BEGIN
 >     OPEN refc FOR SELECT n, takusenTestFunc2(n) from t_natural where n < lim order by n;
 >     RETURN refc;
 > END; $$ LANGUAGE plpgsql;

 > CREATE OR REPLACE FUNCTION takusenTestFunc2(lim int4) RETURNS refcursor AS $$
 > DECLARE refc refcursor;
 > BEGIN
 >     OPEN refc FOR SELECT n from t_natural where n < lim order by n;
 >     RETURN refc;
 > END; $$ LANGUAGE plpgsql;

... then this code shows how nested queries might work:

 > selectNestedMultiResultSet = do
 >   let
 >     q = "SELECT n, takusenTestFunc(n) from t_natural where n < 10 order by n"
 >     iterMain   (i::Int) (c::RefCursor String) acc = result' ((i,c):acc)
 >     iterInner  (i::Int) (c::RefCursor String) acc = result' ((i,c):acc)
 >     iterInner2 (i::Int) acc = result' (i:acc)
 >   withTransaction RepeatableRead $ do
 >     rs <- doQuery (sql q) iterMain []
 >     flip mapM_ rs $ \(outer, c) -> do
 >       rs <- doQuery c iterInner []
 >       flip mapM_ rs $ \(inner, c) -> do
 >         rs <- doQuery c iterInner2 []
 >         flip mapM_ rs $ \i -> do
 >           liftIO (putStrLn (show outer ++ " " ++ show inner ++ " " ++ show i))

Just to make it clear: the outer query returns a result-set that includes
a 'Database.Enumerator.RefCursor' column. Each cursor from that column is passed to
'Database.Enumerator.doQuery' to process it's result-set;
here we use 'Control.Monad.mapM_' to apply an IO action to the list returned by
'Database.Enumerator.doQuery'.

For Oracle the example is slightly different.
The reason it's different is that:

 * Oracle requires that the parent cursor must remain open
   while processing the children
   (in the PostgreSQL example, 'Database.Enumerator.doQuery'
   closes the parent cursor after constructing the list,
   before the list is processed. This is OK because PostgreSQL
   keeps the child cursors open on the server until they are explicitly
   closed, or the transaction or session ends).

 * our current Oracle implementation prevents marshalling
   of the cursor in the result-set buffer to a Haskell value,
   so each fetch overwrites the buffer value with a new cursor.
   This means you have to fully process a given cursor before
   fetching the next one.

Contrast this with the PostgreSQL example above,
where the entire result-set is processed to give a
list of RefCursor values, and then we run a list of actions
over this list with 'Control.Monad.mapM_'.
This is possible because PostgreSQL refcursors are just the
database cursor names, which are Strings, which we can marshal
to Haskell values easily.

 > selectNestedMultiResultSet = do
 >   let
 >     q = "select n, cursor(SELECT nat2.n, cursor"
 >         ++ "     (SELECT nat3.n from t_natural nat3 where nat3.n < nat2.n order by n)"
 >         ++ "   from t_natural nat2 where nat2.n < nat.n order by n)"
 >         ++ " from t_natural nat where n < 10 order by n"
 >     iterMain   (outer::Int) (c::RefCursor StmtHandle) acc = do
 >       rs <- doQuery c (iterInner outer) []
 >       result' ((outer,c):acc)
 >     iterInner outer (inner::Int) (c::RefCursor StmtHandle) acc = do
 >       rs <- doQuery c (iterInner2 outer inner) []
 >       result' ((inner,c):acc)
 >     iterInner2 outer inner (i::Int) acc = do
 >       liftIO (putStrLn (show outer ++ " " ++ show inner ++ " " ++ show i))
 >       result' (i:acc)
 >   withTransaction RepeatableRead $ do
 >     rs <- doQuery (sql q) iterMain []
 >     return ()

Note that the PostgreSQL example can also be written like this
(except, of course, that the actual query text is that
from the PostgreSQL example).



--------------------------------------------------------------------
-- Haddock notes:
--------------------------------------------------------------------

The best way (that I've found) to get a decent introductory/explanatory
section for the module is to break the explanation into named chunks
(these begin with -- $<chunk-name>),
put the named chunks at the end, and reference them in the export list.

You *can* write the introduction inline, as part of the module description,
but Haddock has no way to make headings.
Instead, if you make an explicit export-list then you can use
the "-- *", "-- **", etc, syntax to give section headings.

(Note: if you don't use an explicit export list, then Haddock will use "-- *" etc
comments to make headings. The headings will appear in the docs in the the locations
as they do in the source, as do functions, data types, etc.)

 - One blank line continues a comment block. Two or more end it.
 - The module comment must contain a empty line between "Portability: ..." and the description.
 - bullet-lists:
     - items must be preceded by an empty line.
     - each list item must start with "*".
 - code-sections:
     - must be preceded by an empty line.
     - use " >" rather than @...@, because "@" allows markup translation, where " >" doesn't.
 - @inline code (monospaced font)@
 - /emphasised text/
 - links: "Another.Module", 'someIdentifier' (same module),
   'Another.Module.someIdentifier', <http:/www.haskell.org/haddock>

