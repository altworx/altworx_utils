

# Module aw_map_utils #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Altworx s.r.o.

__Authors:__ Adam Kovari ([`adam.kovari@altworx.com`](mailto:adam.kovari@altworx.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#deep_map_to_proplist-1">deep_map_to_proplist/1</a></td><td>Deep-convert maps into a nested proplist.</td></tr><tr><td valign="top"><a href="#deep_merge_maps-2">deep_merge_maps/2</a></td><td>Deep-merge maps, nested maps on the with the same keys in both maps on input are merged into a single map.</td></tr><tr><td valign="top"><a href="#mapget-4">mapget/4</a></td><td>Map a function over the value in a Map or return default value.</td></tr><tr><td valign="top"><a href="#multimap_from_list-1">multimap_from_list/1</a></td><td>Create a map from a proplists, where unique keys contain list of values.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="deep_map_to_proplist-1"></a>

### deep_map_to_proplist/1 ###

<pre><code>
deep_map_to_proplist(M::#{}) -&gt; [{term(), term()}]
</code></pre>
<br />

Deep-convert maps into a nested proplist

<a name="deep_merge_maps-2"></a>

### deep_merge_maps/2 ###

<pre><code>
deep_merge_maps(M1::#{}, M2::#{}) -&gt; #{}
</code></pre>
<br />

Deep-merge maps, nested maps on the with the same keys in both maps on input are merged into a single map

<a name="mapget-4"></a>

### mapget/4 ###

<pre><code>
mapget(Fun::fun((X::term()) -&gt; Value::V), Key::term(), Map::#{}, Default::D) -&gt; V | D
</code></pre>
<br />

Map a function over the value in a Map or return default value

<a name="multimap_from_list-1"></a>

### multimap_from_list/1 ###

<pre><code>
multimap_from_list(L::list()) -&gt; #{any() =&gt; list()}
</code></pre>
<br />

Create a map from a proplists, where unique keys contain list of values

