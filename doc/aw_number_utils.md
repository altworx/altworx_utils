

# Module aw_number_utils #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2016, Altworx s.r.o.

__Authors:__ Adam Kovari ([`adam.kovari@altworx.com`](mailto:adam.kovari@altworx.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#abs_diff-3">abs_diff/3</a></td><td>Assert whether two numbers are apart by at most an absolute value specified as the third parameter.</td></tr><tr><td valign="top"><a href="#binary_to_number-1">binary_to_number/1</a></td><td>Convert a binary to either float or integer.</td></tr><tr><td valign="top"><a href="#list_to_number-1">list_to_number/1</a></td><td>Convert a list to either float or integer.</td></tr><tr><td valign="top"><a href="#percent_diff-3">percent_diff/3</a></td><td>Assert whether two numbers are apart by at most a percent value specified as the third parameter.</td></tr><tr><td valign="top"><a href="#string_float_or_default-2">string_float_or_default/2</a></td><td>Returns a float value of a string or default value if the input is not a string representing a float.</td></tr><tr><td valign="top"><a href="#string_integer_or_default-2">string_integer_or_default/2</a></td><td>Returns an integer value of a string or default value if the input is not a string representing an integer.</td></tr><tr><td valign="top"><a href="#string_is_float-1">string_is_float/1</a></td><td>Assert whether a list or a binary actually represents a float.</td></tr><tr><td valign="top"><a href="#string_is_integer-1">string_is_integer/1</a></td><td>Assert whether a list or a binary actually represents a integer.</td></tr><tr><td valign="top"><a href="#string_is_numeric-1">string_is_numeric/1</a></td><td>Assert whether a list or a binary actually represents a number (float or integer).</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="abs_diff-3"></a>

### abs_diff/3 ###

<pre><code>
abs_diff(FieldValue::number(), PrevFieldValue::number(), ConditionBody::number()) -&gt; boolean()
</code></pre>
<br />

Assert whether two numbers are apart by at most an absolute value specified as the third parameter

<a name="binary_to_number-1"></a>

### binary_to_number/1 ###

<pre><code>
binary_to_number(B::binary()) -&gt; number()
</code></pre>
<br />

Convert a binary to either float or integer

<a name="list_to_number-1"></a>

### list_to_number/1 ###

<pre><code>
list_to_number(L::list()) -&gt; number()
</code></pre>
<br />

Convert a list to either float or integer

<a name="percent_diff-3"></a>

### percent_diff/3 ###

<pre><code>
percent_diff(FieldValue::number(), PrevFieldValue::number(), ConditionBody::number()) -&gt; boolean()
</code></pre>
<br />

Assert whether two numbers are apart by at most a percent value specified as the third parameter

<a name="string_float_or_default-2"></a>

### string_float_or_default/2 ###

<pre><code>
string_float_or_default(F::list() | binary(), Default) -&gt; float() | Default
</code></pre>
<br />

Returns a float value of a string or default value if the input is not a string representing a float

<a name="string_integer_or_default-2"></a>

### string_integer_or_default/2 ###

<pre><code>
string_integer_or_default(I::list() | binary(), Default) -&gt; integer() | Default
</code></pre>
<br />

Returns an integer value of a string or default value if the input is not a string representing an integer

<a name="string_is_float-1"></a>

### string_is_float/1 ###

<pre><code>
string_is_float(L::list() | binary()) -&gt; boolean()
</code></pre>
<br />

Assert whether a list or a binary actually represents a float. Returns false if the input is neither a list or binary

<a name="string_is_integer-1"></a>

### string_is_integer/1 ###

<pre><code>
string_is_integer(L::list() | binary()) -&gt; boolean()
</code></pre>
<br />

Assert whether a list or a binary actually represents a integer. Returns false if the input is neither a list or binary

<a name="string_is_numeric-1"></a>

### string_is_numeric/1 ###

<pre><code>
string_is_numeric(L::list() | binary()) -&gt; boolean()
</code></pre>
<br />

Assert whether a list or a binary actually represents a number (float or integer). Returns false if the input is neither a list or binary

