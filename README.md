# Memoization Library for Erlang.

A simple implementation of memoization based on ETS.

## Usage example

```
...
ok = application:start(memoiz),
...
%% this call will return after 1 second:
ok = memoiz:do(timer, sleep, [1000]),
...
%% and this call will return almost immediately:
ok = memoiz:do(timer, sleep, [1000]),
...
```

or use functional objects:

```
...
ok = application:start(memoiz),
...
%% this call will return after 1 second:
ok = memoiz:do(fun timer:sleep/1, [1000]),
...
%% and this call will return almost immediately:
ok = memoiz:do(fun timer:sleep/1, [1000]),
...
```

## Configuration

There is two environment variables essential for the library:

* max_size - positive integer or atom 'infinity'. Default is 'infinity'.
 The maximum number of records which will be stored in the library
 storage. When size limit is reached, the least recent item will
 be removed before inserting the new value;
* max_mem - positive integer or atom 'infinity'. Default is 'inifinity'.
 The maximum size of memory used by the library storage. When limit is
 reached, the least recent item (or items) will be removed before
 inserting the new one.
