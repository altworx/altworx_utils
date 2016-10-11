

# Module aw_misc_utils #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Altworx s.r.o.

__Authors:__ Adam Kovari ([`adam.kovari@altworx.com`](mailto:adam.kovari@altworx.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#binary_join-2">binary_join/2</a></td><td>Join binaries with a separator.</td></tr><tr><td valign="top"><a href="#to_binary-1">to_binary/1</a></td><td>Convert item to binary.</td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>Convert item to list.</td></tr><tr><td valign="top"><a href="#tuple_index_of-2">tuple_index_of/2</a></td><td>Index of an item in a tuple.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="binary_join-2"></a>

### binary_join/2 ###

<pre><code>
binary_join(Tail::[binary()], Sep::binary()) -&gt; binary()
</code></pre>
<br />

Join binaries with a separator

<a name="to_binary-1"></a>

### to_binary/1 ###

<pre><code>
to_binary(B::binary() | number() | atom() | iolist()) -&gt; binary()
</code></pre>
<br />

Convert item to binary

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(L::list() | binary() | number() | atom() | iolist()) -&gt; list()
</code></pre>
<br />

Convert item to list

<a name="tuple_index_of-2"></a>

### tuple_index_of/2 ###

<pre><code>
tuple_index_of(Item::term(), Tuple::tuple() | list()) -&gt; non_neg_integer()
</code></pre>
<br />

Index of an item in a tuple

