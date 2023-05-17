module ExampleDb (
    ExamplePart(..),
    Example(..), 
    ExampleDb(..)) where

data ExamplePart stringType = Code { title::stringType, code::stringType } 
    | Markup { markup::stringType }

data Example idType stringType = Example {
    id :: idType,
    alias :: stringType,
    name :: stringType,
    parts :: [ExamplePart stringType]
}

data ExampleDb md idType stringType = ExampleDb {
    categories :: md [stringType],
    examplesByCategory :: stringType -> md (Maybe [Example idType stringType]),
    exampleById :: idType -> md (Maybe (Example idType stringType)),
    exampleByCategoryAndAlias :: stringType -> stringType -> md (Maybe (Example idType stringType)),
    searchForExamples :: stringType -> md [(stringType, Example idType stringType)]
}