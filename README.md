# hs-graphql

wip

## components of the type system

### types
This is the public api through which haskell data types may implement a GraphQL type of some kind.
This is done  by implementing an instance of `GraphQLType`:

```hs
newtype ID = ID String deriving (FromJSON, ToJSON)

instance GraphQLType ID where
  type KindOf ID = GraphQLScalar
  typename _ = "ID"
```

### kinds
A graphql kind is a proxy type, such as the `GraphQLScalar` in the example above, that generally carries a dictionary of instances from its carrier type to be used for resolution and introspection.
Type kinds can also be extended by implementing `GraphQLKind`, which translates its argument into a graphql type definition, and `GraphQLTypeable` which defines how to instantiate the proxy data type:
```hs
data GraphQLScalar a where
  Scalar :: (FromJSON a, ToJSON a) => Scalar a

instance GraphQLKind GraphQLScalar where
  type Kind GraphQLScalar = SCALAR
  typeDef Scalar = ScalarDef
instance (FromJSON a , ToJSON a) => GraphQLTypeable GraphQLScalar a where
  typeOf = Scalar
```
To plug its carrier type into an executable resolver it must further implement `GraphQLInputKind`, `GraphQLOutputKind`, or both depending on its `Kind` type. Such is the case for scalars:
```hs
instance GraphQLInputKind GraphQLScalar where
  readInputType Scalar = fromJSON
instance GraphQLOutputKind m GraphQLScalar where
  resolve Scalar = Leaf . toJson
```

## todo

- [ ] kinds
  - [x] scalars
  - [ ] enums
  - [x] objects
  - [x] input objects
  - [ ] unions
  - [ ] interfaces
  - [x] lists
  - [x] nullable
- [ ] parsing
- [x] validation
- [x] resolvers
  - [x] resolve objects recursively
  - [x] apply inputs
  - [ ] error handling
  - [ ] batching
  - [ ] subscriptions
- [ ] root resolver
  - [ ] map operation types to their respective resolvers
  - [ ] extract all types defined in the resolvers recursively
- [ ] schema introspection
