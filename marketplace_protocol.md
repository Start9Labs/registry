# Marketplace Protocool Documentation

## Terms

- PkgId: string comprised of lowercase letters, numbers and -'s, must begin with letter, must not end with -, ex: ride-the-lightning
- S9PK: "<PkgId>.s9pk"
- Emver: string that matches the following regex: \d((\.\d){2})((\d\.)?)
- EmverMod: ">" OR "<" OR "=" OR ">=" OR "<=" OR "!="
- EmverRange: <EmverMod><Emver> OR "\*" OR "!"

## Routes

### GET /info

#### Query Params

NONE

#### Returns

```json
{
  // this is the name of the marketplace and will show in the marketplace view on EOS
  "name": "<STRING>",
  "categories": ["<STRING>"]
}
```

#### Requirements

- categories must be lowercase.
- all category tags that appear in /index MUST appear here

### GET /index

#### Query Params

- (Required) "eos-version-compat": EmverRange specifying what versions of the OS that pack the packages are valid response items
- (Optional) "ids": json array of package ids
- (Optional) "category": string representing
- (Optional) "page": which page of the index you want (NOTE: starts at 1) (Default: 1)
- (Optional) "per-page": size of the page you want (Default: 20)

#### Returns

```json
[
    {
        // package icon at most recent version
        "icon": "<DATAURL>",
        // license at most recent version
        "license": "<STRING>",
        // instructions at most recent version
        "instructions": "<STRING>",
        // categories at most recent version
        "categories": ["<STRING>",...],
        // all valid versions
        "versions": ["<Emver>",...],
        // package dependencies
        "dependency-metadata": {
            // dependency package id
            "<PkgId>": {
                // name of dependency
                "title": "<STRING>",
                // icon of dependency
                "icon": "<DATAURL>"
            },
            ...
        }
    },
    ...
]
```

#### Requirements

- if "ids" is specified, MUST NOT return packages not in that list
- if "category" is specified, MUST NOT return packages that do not fit in that category
- MUST NOT return more than "per-page" number of packages
- all packages returned MUST be packed by an OS satisfying "eos-version-compat"
- icon MUST be a data url of the icon data
- dependency entries MUST be specified in the manifest of the package
- categories SHOULD include the categories specified but the package
- categories MAY be at the discretion of the marketplace maintainer

### GET /latest

#### Query Params

- (Required) "ids": json array of package ids

#### Returns

```json
{
    "<PkgId>": null | "<Emver>"
    ...
}
```

#### Requirements

- set of package ids in the response key set MUST be the same as the set of package ids in the query string
- if the package is not available on the marketplace its entry MUST be `null`
- if the package is available on the marketplace then its entry MUST be the highest Emver for that package

### GET /:S9PK

#### Query Params

- (Optional) "spec": Emver
- (Optional) "version-priority": "min" OR "max"

#### Returns

binary stream of the s9pk package data

#### Requirements

- the package bytes supplied MUST be the same as the package id specified in the URL.
- Content-Length MUST be specified
- if "spec" is specified, the package version returned MUST satisfy the spec in the query string
- if "version-priority" is specified to "min" AND multiple packages satisfy "spec", version returned MUST be the smallest satisfactory package version
- if "version-priority" is specified to "max" AND multiple packages satisfy "spec", version returned MUST be the largest satisfactory package version

### GET /manifest/:PkgId

#### Query Params

- (Optional) "spec": Emver
- (Optional) "version-priority": "min" OR "max"

#### Returns

raw json document of the package's manifest

#### Requirements

- the manifest supplied MUST be the same as the package id specified in the URL.
- Content-Length MUST be specified
- if "spec" is specified, the package version returned MUST satisfy the spec in the query string
- if "version-priority" is specified to "min" AND multiple packages satisfy "spec", version returned MUST be the smallest satisfactory package version
- if "version-priority" is specified to "max" AND multiple packages satisfy "spec", version returned MUST be the largest satisfactory package version

### GET /release-notes/:PkgId

#### Query Params

NONE

#### Returns

```json
{
    // release notes for each version
    "<Emver>": "<STRING>"
    ...
}
```

#### Requirements

- All keys in this response MUST be available versions for the package
- the value for each key MUST be the release notes for that version

### GET /icon/:PkgId

#### Query Params

- (Optional) "spec": Emver
- (Optional) "version-priority": "min" OR "max"

#### Returns

raw icon binary of the package

#### Requirements

- the icon supplied MUST be the same as the package id specified in the URL.
- Content-Length MUST be specified
- if "spec" is specified, the package version returned MUST satisfy the spec in the query string
- if "version-priority" is specified to "min" AND multiple packages satisfy "spec", version returned MUST be the smallest satisfactory package version
- if "version-priority" is specified to "max" AND multiple packages satisfy "spec", version returned MUST be the largest satisfactory package version
- Content-Type MUST be specified indicating what image type it is

### GET /license/:PkgId

#### Query Params

- (Optional) "spec": Emver
- (Optional) "version-priority": "min" OR "max"

#### Returns

raw license file of the package

#### Requirements

- the icon supplied MUST be the same as the package id specified in the URL.
- Content-Length MUST be specified
- if "spec" is specified, the package version returned MUST satisfy the spec in the query string
- if "version-priority" is specified to "min" AND multiple packages satisfy "spec", version returned MUST be the smallest satisfactory package version
- if "version-priority" is specified to "max" AND multiple packages satisfy "spec", version returned MUST be the largest satisfactory package version

### GET /instructions/:PkgId

#### Query Params

- (Optional) "spec": Emver
- (Optional) "version-priority": "min" OR "max"

#### Returns

raw instructions file of the package

#### Requirements

- the instructions supplied MUST be the same as the package id specified in the URL.
- Content-Length MUST be specified
- if "spec" is specified, the package version returned MUST satisfy the spec in the query string
- if "version-priority" is specified to "min" AND multiple packages satisfy "spec", version returned MUST be the smallest satisfactory package version
- if "version-priority" is specified to "max" AND multiple packages satisfy "spec", version returned MUST be the largest satisfactory package version

### GET /version/:PkgId

#### Query Params

- (Optional) "spec": Emver
- (Optional) "version-priority": "min" OR "max"

#### Returns

```json
{ "version": "<EMVER>" }
```

#### Requirements

- the version supplied MUST be a valid version for the package id specified in the URL
- if "spec" is specified, the package version returned MUST satisfy the spec in the query string
- if "version-priority" is specified to "min" AND multiple packages satisfy "spec", version returned MUST be the smallest satisfactory package version
- if "version-priority" is specified to "max" AND multiple packages satisfy "spec", version returned MUST be the largest satisfactory package version
