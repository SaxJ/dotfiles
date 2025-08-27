# This is a comment
POST https://api.healthengine.com.au/graphql

```graphql
# Commanet
query($thing: String!) {
    practice(id: 42118, name: "foo", wow: $thing) {
        id
    }
}
```

GET https://google.com
