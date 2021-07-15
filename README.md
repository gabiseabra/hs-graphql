# hs-graphql

wip

## components of the type system

### types
this is the public api through which haskell data types may implement a graphql type of some kind.
this is done  by implementing an instance of `GraphQLTypeable`:
```hs
data ID = ID Int

instance GraphQLTypeable ID where
  type KindOf ID = Scalar
  typename _ = "ID"
```

### kinds
a graphql kind is a proxy type such as `Scalar` in the example above that generally carries a dictionary from its argument.
type kinds can also be extended by implementing `GraphQLKind`, which will translate its argument into a graphql type definition:
```hs
data Scalar a where
  Scalar :: (FromJSON a, ToJSON a) => Scalar a

instance GraphQLKind Scalar where
  type Kin Scalar = SCALAR
  typeDef Scalar = ScalarDef
```
To plug its carrier type into an executable resolver it must further implement `GraphQLInputKind`, `GraphQLOutputKind`, or both depending on its `Kin` type. such is the case for scalars: 
```hs
instance GraphQLInputKind Scalar where
  readInputType Scalar = fromJSON # this is now the actual impl
instance GraphQLOutputKind m Scalar where
  resolve Scalar = pure . toJson
```

## todo

- [ ] kinds
  - [x] scalars
  - [ ] enums
  - [x] objects
  - [ ] input objects
  - [ ] unions
  - [ ] interfaces
  - [ ] lists
  - [ ] nullable
- [ ] parsing
  - [ ] parse selection from string
  - [ ] validation
- [x] resolvers
  - [x] resolve objects recursively
  - [ ] apply inputs
  - [ ] error handling
- [ ] root resolver
  - [ ] map operation types to their respective resolvers
  - [ ] extract all types defined in the resolvers recursively
- [ ] schema introspection
