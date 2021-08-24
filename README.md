# just-json-server

Run a API server supporting CRUD with just a json file.

## Prerequisite

Executables can be found on [github release](https://github.com/chauchakching/just-json-server/releases) page

### Build from source

Haskell build tool `stack`

## Getting started

Create a json file with name, say `db.json`

```json
{
  "articles": [
    {
      "id": 1,
      "title": "title 1"
    },
    {
      "id": 2,
      "title": "title 2"
    }
  ],
  "posts": [
    {
      "id": 1,
      "title": "post 1"
    }
  ]
}
```

Then, build and start an API server

```bash
# build from source code
stack build

# run the executable
stack exec just-json-server db.json
```

which supports CRUD operations on the given data, e.g.

```bash
# get all
GET /articles

# get one
GET /articles/1

# create
POST /articles

# update
PATCH /articles/1
PUT /articles/1

# delete
DELETE /articles/1
```

## Development

Start api server with [steeloverseer](https://github.com/schell/steeloverseer) to watch file changes and restart automatically

## Notes

CI scripts referenced from [purescript](https://github.com/purescript/purescript)

## Why?

Because Haskell rocks!

This kind of json api server is super easy to implement in dynamic languages such as Nodejs. See [json-server](https://github.com/typicode/json-server). Nonetheless, it's a very good way to practice writing Haskell code by re-implementing existing libraries. 

During development, the only bugs I got were

- accidentally passing resource name value (string) instead of resource id (also string) to some function
- list calculation `replaceOrInsertElem`, which have to be verified by test cases

Other than that, when the program compiles, it is correct.

Performance-wise, re-implementing it with Haskell is rediculous, because the performance should be bounded by IO of the json file.
