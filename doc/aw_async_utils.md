

# Module aw_async_utils #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2017, Altworx s.r.o.

__Authors:__ Adam Kovari ([`adam.kovari@altworx.com`](mailto:adam.kovari@altworx.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#execute_all-2">execute_all/2</a></td><td>Asynchronously execute tasks and return a list of all results.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="execute_all-2"></a>

### execute_all/2 ###

<pre><code>
execute_all(Funs::fun(() -&gt; T), Timeout::timeout()) -&gt; [T]
</code></pre>
<br />

Asynchronously execute tasks and return a list of all results.
Order of the results is undefined.

