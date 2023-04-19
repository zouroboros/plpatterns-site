module ExampleDb (
    Example(Example, id, name, example, description), 
    ExampleDb(ExampleDb, categories, examplesByCategory, exampleById)) where

data Example idType stringType = Example {
    id :: idType,
    name :: stringType,
    example :: stringType,
    description :: stringType
}

data ExampleDb md idType stringType = ExampleDb {
    categories :: md [stringType],
    examplesByCategory :: stringType -> md (Maybe [Example idType stringType]),
    exampleById :: idType -> md (Maybe (Example idType stringType))
}