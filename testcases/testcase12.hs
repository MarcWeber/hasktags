-- to be found MultiLineTestCase
module MultiLineTestCase where

-- to be found Foo

class (A x, B x)
      => Foo x where

-- to be found Bar

class (A x, B x)
      =>
      Bar x where

-- to be found Baz

class (A x, B x) =>
      Baz x where
