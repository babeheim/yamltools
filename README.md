yamltools
============

# JSON List Basics

The JSON syntax distinguishes two kinds of collections, "arrays" and "objects". In R, these correspond roughtly to named and unnamed lists.

## JSON Arrays

Here is a simple array of numeric values:

```json
[10, 12, 15]
```

Arrays of multiple values are separated by commas. Here is a simple array of character values:

```json
["alex", "bob", "susan"]
```

Because they are just collections of values, JSON arrays have to be navigated by using the value index number, e.g. the 2nd entry in the above array is "bob". The values do not have to be all the same type either. Here is an array with all the 'simple' values (numeric, character, logical, and null):

```json
[12, null, true, "quick fox"]
``` 

Since JSON values include arrays, we can put arrays inside arrays

```json
[12, null, true, [10, 12, 15], "quick fox"]
```

Let's go nuts:

```json
[10, [12, null, true, [10, 12, 15], "quick fox"], 15, [10, 12, 15]]
```

Since this gets quite hard for humans to parse, it helps to use whitespace (the computer doesn't care).

```json
[
  10,
  [
    12,
    null,
    true,
    [
      10,
      12,
      15
    ],
    "quick fox"
  ],
  15,
  [
    10,
    12,
    15
  ]
]
```

Note that in R, we could represent this as an unnamed list:

```R
x <- list(
  10,
  list(
    12,
    NULL,
    TRUE,
    list(
      10,
      12,
      15
    ),
    "quick fox"
  ),
  15,
  list(
    10,
    12,
    15
  )
)
```

Just as in the JSON array, we would have to use index values to navigate this R list, e.g `x[[2]][[1]]` is 12.

## JSON Objects

In contrast, JSON objects are collections of key-value pairs. The key is always a character, but its corresponding value can be any kind of JSON value, including other JSON objects or arrays.

```json
{"name": "Alex", "age": 10, "owns car": true, "favorite colors": ["blue", "red", "teal"]}
```

As above, it is easier for humans to read by using whitespace:

```json
{
  "name"           : "Alex",
  "age"            : 10,
  "owns car"       : false,
  "favorite colors":
    [
      "blue",
      "red",
      "teal"
    ]
}
```

A JSON object with other objects inside it might look like so:


```json
{
  "name"           : "Alex",
  "age"            : 10,
  "owns car"       : false,
  "favorite colors": [
    "blue",
    "red",
    "teal"
  ],
  "anthropometrics": {
    "height": 110,
    "weight": 50
  }
}
```

In Javascript, which developed JSON, entries in an object can be addressed by their keys. `.anthropometrics.height` is 110. `.favorite colors[1]` is "blue".

And, as we commonly see, objects holding arrays holding collections of objects:


```json
{
  "name"           : "Alex",
  "age"            : 10,
  "owns car"       : false,
  "favorite colors": [
    "blue",
    "red",
    "teal"
  ],
  "anthropometrics": {
    "height": 110,
    "weight": 50
  },
  "pets": [
    {
      "name": "benji",
      "species": "dog"
    },
    {
      "name": "garfield",
      "species": "cat"
    },
    {
      "name": "polly",
      "species": "golfish"
    }
  ]
}
```

Of course this entry can be nested inside another, larger array, e.g. a collection of residents in a household, and a collection of households within a neighborhood, etc. It's completely recursive.

The equivalent representation in R of a JSON object is a named list, e.g.

```r
list(
  "name"           = "Alex",
  "age"            = 10,
  "owns car"       = FALSE,
  "favorite colors"= list(
    "blue",
    "red",
    "teal"
  ),
  "anthropometrics"= list(
    "height"= 110,
    "weight"= 50
  ),
  "pets"= list(
    list(
      "name"   = "benji",
      "species"= "dog"
    ),
    list(
      "name"   = "garfield",
      "species"= "cat"
    ),
    list(
      "name"   = "polly",
      "species"= "golfish"
    )
  )
)
```

## The Basic Problem with R Lists

R doesn't have a clean distinction between named and unnamed lists. One could write this in R, which has no JSON equivalent:

```R
x <- list(
  "name" = "Alex",
  "age" = 10,
  TRUE,
  42
)
```

The reason this works in R is that, fundamentally, *all lists are unnamed*, but for named lists a second vector of names is stored along with the unnamed lists.

This leads to some conceptual confusion about what, exactly, can go on the right side of an equal sign. Here is another entry in R which has no equivalent in JSON:

```R
list("name" = ("Alex" = 10))
```

Note that R evaluates this as identical to

```R
list("name" = 10)
```

This leads to some rather silly behavior:

```R
list(
  "name" = (
    "Alex" = (
      "age" = (
        10
      )
    )
  )
)
```

JSON doesn't allow such shenannigans because a key-value pair is not itself a value. Any key-value pair must be itself inside an object, `{ }`, which can be a value. This allows a relatively straightforwards way to represent nested information.

