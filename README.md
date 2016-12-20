# simple_cache
Simple erlang cache system that stores key value pairs, with each unique key referencing one value

With our simple cache server, we can perform the following:

        - place a key and a value into the cache
        - using a key to retrieve a value
        - using a key to delete a key value pair
        
USAGE

To compile the erl files in the src directory,

cd ebin
erlc ../src/*.erl

in the ebin directory, run

erl

Then, run the following in the Erlang shell:

1> application:start(simple_cache).

ok
