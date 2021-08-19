# Eflatbuffers
=====

An OTP library

Build
-----
    $ rebar3 compile


This is a [flatbuffers](https://google.github.io/flatbuffers/) implementation in Erlang.

In contrast to existing implementations there is no need to compile code from a schema. Instead, data and schemas are processed dynamically at runtime, offering greater flexibility.

## Using Eflatbuffers

Schema file:
```
table Root {
  foreground:Color;
  background:Color;
}

table Color {
  red:   ubyte;
  green: ubyte;
  blue:  ubyte;
}
root_type Root;
```

Parsing the schema:
```erlang
1> {ok, Schema} = file:read_file(path_to_schema).
{ok,<<"table Root {\r\n  foreground:Color;\r\n  background:Color;\r\n}\r\n\r\ntable Color {\r\n  red:   ubyte;\r\n  green: ubyte;"...>>}
2> parser:parse_schema(Schema).
{#{'Color' =>
       {table,#{fields =>
                    [{red,{ubyte,#{default => 0}}},
                     {green,{ubyte,#{default => 0}}},
                     {blue,{ubyte,#{default => 0}}}],
                indices =>
                    #{blue => {2,{ubyte,#{default => 0}}},
                      green => {1,{ubyte,#{default => 0}}},
                      red => {0,{ubyte,#{default => 0}}}}}},
   'Root' =>
       {table,#{fields =>
                    [{foreground,{table,#{name => 'Color'}}},
                     {background,{table,#{name => 'Color'}}}],
                indices =>
                    #{background => {1,{table,#{name => 'Color'}}},
                      foreground => {0,{table,#{name => 'Color'}}}}}}},
 #{root_type => 'Root'}}

```

Serializing data:

```erlang
3> ColorScheme = #{foreground => #{red => 128, green => 20, blue => 255}, background => #{red => 0, green => 100, blue => 128}}.
#{background => #{blue => 128,green => 100,red => 0},
  foreground => #{blue => 255,green => 20,red => 128}}
4> {ok, ColorSchemeFb} = eflatbuffers:write(ColorScheme, Schema).
{ok, <<16, 0, 0, 0, 0, 0, 0, 0, 8, 0, 12, 0, 4, 0, 8, 0, 8, 0, 0, 0, 18, 0, 0, 0, 31,
  0, 0, ...>>}
```

So we can `read` the whole thing which converts it back into a map:

```erlang
5> eflatbuffers:read(ColorSchemeFb, Schema).
{ok,#{background => #{blue => 128,green => 100,red => 0},
      foreground => #{blue => 255,green => 20,red => 128}}}
```

### features in Eflatbufers 

* tables
* scalars
* strings
* vectors
* unions
* enums
* defaults
* json to fb
* fb to json
* file identifier + validation
* random access
* validate file identifiers
* vectors of enums
