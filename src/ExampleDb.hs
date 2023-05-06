module ExampleDb (
    Example(..), 
    ExampleDb(..)) where

data Example idType stringType = Example {
    id :: idType,
    alias :: stringType,
    name :: stringType,
    example :: stringType,
    description :: stringType
}

data ExampleDb md idType stringType = ExampleDb {
    categories :: md [stringType],
    examplesByCategory :: stringType -> md (Maybe [Example idType stringType]),
    exampleById :: idType -> md (Maybe (Example idType stringType)),
    exampleByCategoryAndAlias :: stringType -> stringType -> md (Maybe (Example idType stringType)),
    searchForExamples :: stringType -> md [(stringType, Example idType stringType)]
}