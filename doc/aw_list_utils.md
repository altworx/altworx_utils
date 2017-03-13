

# Module aw_list_utils #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Altworx s.r.o.

__Authors:__ Adam Kovari ([`adam.kovari@altworx.com`](mailto:adam.kovari@altworx.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#mapfirst-3">mapfirst/3</a></td><td>Map a function over the first element matching a filter.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="mapfirst-3"></a>

### mapfirst/3 ###

<pre><code>
mapfirst(MapF::fun((E) -&gt; Res), FilterF::fun((E) -&gt; boolean()), L::[E]) -&gt; Res | nomatch
</code></pre>
<br />

Map a function over the first element matching a filter

