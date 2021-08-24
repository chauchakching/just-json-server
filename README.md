# just-json-server

Command to run a API server supporting CRUD with just a json file.

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

## Notes

CI scripts referenced from [purescript](https://github.com/purescript/purescript)