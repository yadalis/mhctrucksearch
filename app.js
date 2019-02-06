(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.bF.aK === region.bX.aK)
	{
		return 'on line ' + region.bF.aK;
	}
	return 'on lines ' + region.bF.aK + ' through ' + region.bX.aK;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




/**_UNUSED/
function _Json_errorToString(error)
{
	return elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? elm$core$Result$Ok(value)
		: (value instanceof String)
			? elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList === 'function' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.d9,
		impl.fI,
		impl.fg,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.dQ.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done(elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done(elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.dQ.b, xhr)); });
		elm$core$Maybe$isJust(request.G) && _Http_track(router, xhr, request.G.a);

		try {
			xhr.open(request.A, request.fJ, true);
		} catch (e) {
			return done(elm$http$Http$BadUrl_(request.fJ));
		}

		_Http_configureRequest(xhr, request);

		request.dj.a && xhr.setRequestHeader('Content-Type', request.dj.a);
		xhr.send(request.dj.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.v; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.D.a || 0;
	xhr.responseType = request.dQ.d;
	xhr.withCredentials = request.ap;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? elm$http$Http$GoodStatus_ : elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		fJ: xhr.responseURL,
		cN: xhr.status,
		fa: xhr.statusText,
		v: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return elm$core$Dict$empty;
	}

	var headers = elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3(elm$core$Dict$update, key, function(oldValue) {
				return elm$core$Maybe$Just(elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2(elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, elm$http$Http$Sending({
			e_: event.loaded,
			e1: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2(elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, elm$http$Http$Receiving({
			eL: event.loaded,
			e1: event.lengthComputable ? elm$core$Maybe$Just(event.total) : elm$core$Maybe$Nothing
		}))));
	});
}

function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return elm$core$Maybe$Nothing;
	}
}



// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2(elm$json$Json$Decode$map, func, handler.a)
				:
			A3(elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		V: func(record.V),
		bG: record.bG,
		bz: record.bz
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.V;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.bG;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.bz) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.d9,
		impl.fI,
		impl.fg,
		function(sendToApp, initialModel) {
			var view = impl.fM;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.d9,
		impl.fI,
		impl.fg,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.aN && impl.aN(sendToApp)
			var view = impl.fM;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.dj);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.fy) && (_VirtualDom_doc.title = title = doc.fy);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.ew;
	var onUrlRequest = impl.ex;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		aN: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.cz === next.cz
							&& curr.b8 === next.b8
							&& curr.cv.a === next.cv.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
					));
				}
			});
		},
		d9: function(flags)
		{
			return A3(impl.d9, flags, _Browser_getUrl(), key);
		},
		fM: impl.fM,
		fI: impl.fI,
		fg: impl.fg
	});
}

function _Browser_getUrl()
{
	return elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return elm$core$Result$isOk(result) ? elm$core$Maybe$Just(result.a) : elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { d2: 'hidden', dv: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { d2: 'mozHidden', dv: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { d2: 'msHidden', dv: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { d2: 'webkitHidden', dv: 'webkitvisibilitychange' }
		: { d2: 'hidden', dv: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail(elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		cJ: _Browser_getScene(),
		cW: {
			bk: _Browser_window.pageXOffset,
			bl: _Browser_window.pageYOffset,
			aE: _Browser_doc.documentElement.clientWidth,
			as: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		aE: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		as: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			cJ: {
				aE: node.scrollWidth,
				as: node.scrollHeight
			},
			cW: {
				bk: node.scrollLeft,
				bl: node.scrollTop,
				aE: node.clientWidth,
				as: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			cJ: _Browser_getScene(),
			cW: {
				bk: x,
				bl: y,
				aE: _Browser_doc.documentElement.clientWidth,
				as: _Browser_doc.documentElement.clientHeight
			},
			dL: {
				bk: x + rect.left,
				bl: y + rect.top,
				aE: rect.width,
				as: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});


// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.cj) { flags += 'm'; }
	if (options.bQ) { flags += 'i'; }

	try
	{
		return elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? elm$core$Maybe$Just(submatch)
				: elm$core$Maybe$Nothing;
		}
		out.push(A4(elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch
				? elm$core$Maybe$Just(submatch)
				: elm$core$Maybe$Nothing;
		}
		return replacer(A4(elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$EQ = 1;
var elm$core$Basics$GT = 2;
var elm$core$Basics$LT = 0;
var elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$List$cons = _List_cons;
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0;
	return elm$core$Dict$keys(dict);
};
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Basics$toFloat = _Basics_toFloat;
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Basics$eq = _Utils_equal;
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$add = _Basics_add;
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$gt = _Utils_gt;
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Basics$sub = _Basics_sub;
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.n) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.p),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.p);
		} else {
			var treeLen = builder.n * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.q) : builder.q;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.n);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.p) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.p);
		}
	});
var elm$core$Basics$False = 1;
var elm$core$Basics$idiv = _Basics_idiv;
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{q: nodeList, n: (len / elm$core$Array$branchFactor) | 0, p: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var elm$core$Maybe$Nothing = {$: 1};
var elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var elm$core$Basics$True = 0;
var elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$append = _Utils_append;
var elm$core$Basics$or = _Basics_or;
var elm$core$Char$toCode = _Char_toCode;
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$String$all = _String_all;
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$uncons = _String_uncons;
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 1) {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$json$Json$Decode$map2 = _Json_map2;
var NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom = elm$json$Json$Decode$map2(elm$core$Basics$apR);
var elm$json$Json$Decode$field = _Json_decodeField;
var NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required = F3(
	function (key, valDecoder, decoder) {
		return A2(
			NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
			A2(elm$json$Json$Decode$field, key, valDecoder),
			decoder);
	});
var author$project$Model$APU = 16;
var author$project$Model$BodyType = 10;
var author$project$Model$BrakeType = 19;
var author$project$Model$CDL = 17;
var author$project$Model$Condition = 0;
var author$project$Model$EngineHP = 22;
var author$project$Model$EngineMake = 7;
var author$project$Model$ExhaustType = 20;
var author$project$Model$FleetCode = 13;
var author$project$Model$FrontAxleWeight = 26;
var author$project$Model$FrontWheelSize = 32;
var author$project$Model$InventoryAge = 28;
var author$project$Model$LocationName = 29;
var author$project$Model$Make = 3;
var author$project$Model$MakeModel = 4;
var author$project$Model$Mileage = 25;
var author$project$Model$OwningBranch = 15;
var author$project$Model$Photo = 18;
var author$project$Model$Price = 21;
var author$project$Model$RearAxleRatio = 30;
var author$project$Model$RearAxleType = 11;
var author$project$Model$RearAxleWeight = 27;
var author$project$Model$RearWheelSize = 31;
var author$project$Model$SalesStatus = 1;
var author$project$Model$SleeperBunk = 6;
var author$project$Model$SleeperInches = 23;
var author$project$Model$SleeperRoof = 5;
var author$project$Model$SpecialFinancing = 14;
var author$project$Model$Suspension = 9;
var author$project$Model$TransType = 8;
var author$project$Model$TruckType = 12;
var author$project$Model$WheelBase = 24;
var author$project$Model$Year = 2;
var author$project$BusinessFunctions$SearchFilterFunctions$partialSearchFiltersMetadata = _List_fromArray(
	[
		{i: 'Condition', g: 'cn', h: 0, j: 'Condition'},
		{i: 'Fleet Code', g: 'fc', h: 13, j: 'FleetCode'},
		{i: 'Sales Status', g: 'ss', h: 1, j: 'SalesStatus'},
		{i: 'Truck Status', g: 'tt', h: 12, j: 'TruckType'},
		{i: 'Special Financing', g: 'sf', h: 14, j: 'SpecialFinancing'},
		{i: 'Price', g: 'pr', h: 21, j: 'Price'},
		{i: 'Mileage', g: 'ml', h: 25, j: 'Mileage'},
		{i: 'Year', g: 'yr', h: 2, j: 'Year'},
		{i: 'Make', g: 'mk', h: 3, j: 'Make'},
		{i: 'Model', g: 'mm', h: 4, j: 'MakeModel'},
		{i: 'Engine', g: 'em', h: 7, j: 'EngineMake'},
		{i: 'HP', g: 'eh', h: 22, j: 'EngineHP'},
		{i: 'Transmission', g: 'ts', h: 8, j: 'TransType'},
		{i: 'Sleeper Roof', g: 'sr', h: 5, j: 'SleeperRoof'},
		{i: 'Sleeper Bunk', g: 'sb', h: 6, j: 'SleeperBunk'},
		{i: 'Sleeper Size', g: 'si', h: 23, j: 'SleeperInches'},
		{i: 'Suspension', g: 'su', h: 9, j: 'Suspension'},
		{i: 'Wheel Base', g: 'wb', h: 24, j: 'WheelBase'},
		{i: 'Rear Axle Type', g: 'ra', h: 11, j: 'RearAxleType'},
		{i: 'Rear Axle Ratio', g: 'rr', h: 30, j: 'RearAxleRatio'},
		{i: 'Front Axle Weight', g: 'fw', h: 26, j: 'FrontAxleWeight'},
		{i: 'Rear Axle Weight', g: 'rw', h: 27, j: 'RearAxleWeight'},
		{i: 'Rear Wheel Size', g: 'rs', h: 31, j: 'RearWheelSize'},
		{i: 'Front Wheel Size', g: 'fs', h: 32, j: 'FrontWheelSize'},
		{i: 'Brake Type', g: 'bk', h: 19, j: 'BrakeType'},
		{i: 'Exhaust Type', g: 'et', h: 20, j: 'ExhaustType'},
		{i: 'Body Type', g: 'bt', h: 10, j: 'BodyType'},
		{i: 'APU', g: 'ap', h: 16, j: 'APU'},
		{i: 'CDL', g: 'cd', h: 17, j: 'CDL'},
		{i: 'Photo', g: 'hp', h: 18, j: 'HasPhoto'},
		{i: 'Inventory Age', g: 'ia', h: 28, j: 'InventoryAge'},
		{i: 'Location Name', g: 'lc', h: 29, j: 'LocationName'},
		{i: 'Owning Branch', g: 'ob', h: 15, j: 'OwningBranch'}
	]);
var elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return elm$core$Maybe$Just(
				f(value));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var elm_community$list_extra$List$Extra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			if (!list.b) {
				return elm$core$Maybe$Nothing;
			} else {
				var first = list.a;
				var rest = list.b;
				if (predicate(first)) {
					return elm$core$Maybe$Just(first);
				} else {
					var $temp$predicate = predicate,
						$temp$list = rest;
					predicate = $temp$predicate;
					list = $temp$list;
					continue find;
				}
			}
		}
	});
var author$project$Commands$convertStringToRangeSearchFilter = function (filterStr) {
	return A2(
		elm$core$Maybe$withDefault,
		21,
		A2(
			elm$core$Maybe$map,
			function (_n1) {
				var filterNameString = _n1.a;
				var filterName = _n1.b;
				return filterName;
			},
			A2(
				elm_community$list_extra$List$Extra$find,
				function (_n0) {
					var filterNameString = _n0.a;
					var filterName = _n0.b;
					return _Utils_eq(filterNameString, filterStr);
				},
				A2(
					elm$core$List$map,
					function (rf) {
						return _Utils_Tuple2(rf.j, rf.h);
					},
					author$project$BusinessFunctions$SearchFilterFunctions$partialSearchFiltersMetadata))));
};
var elm$json$Json$Decode$succeed = _Json_succeed;
var author$project$Commands$searchFilterRangeUnionTypeString = function (str) {
	return elm$json$Json$Decode$succeed(
		author$project$Commands$convertStringToRangeSearchFilter(str));
};
var elm$json$Json$Decode$andThen = _Json_andThen;
var elm$json$Json$Decode$string = _Json_decodeString;
var author$project$Commands$searchFilterRangeUnionTypeDecoder = A2(elm$json$Json$Decode$andThen, author$project$Commands$searchFilterRangeUnionTypeString, elm$json$Json$Decode$string);
var elm$core$String$toLower = _String_toLower;
var elm$json$Json$Decode$fail = _Json_fail;
var author$project$Commands$stringBoolDecoder = A2(
	elm$json$Json$Decode$andThen,
	function (val) {
		var _n0 = elm$core$String$toLower(val);
		switch (_n0) {
			case 'true':
				return elm$json$Json$Decode$succeed(true);
			case 'false':
				return elm$json$Json$Decode$succeed(false);
			default:
				return elm$json$Json$Decode$fail('Expecting \"true\" or \"false\" but found ' + val);
		}
	},
	elm$json$Json$Decode$string);
var author$project$Model$SearchFilterType = F6(
	function (index, searchFilterKey, searchFilterExtraData, userAction, resultCount, filterCategory) {
		return {dV: filterCategory, bv: index, cG: resultCount, eX: searchFilterExtraData, al: searchFilterKey, fK: userAction};
	});
var elm$json$Json$Decode$int = _Json_decodeInt;
var author$project$Commands$searchFilterDecoder = A3(
	NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'filterCategory',
	author$project$Commands$searchFilterRangeUnionTypeDecoder,
	A3(
		NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'resultCount',
		elm$json$Json$Decode$int,
		A3(
			NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'userAction',
			author$project$Commands$stringBoolDecoder,
			A3(
				NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'searchFilterExtraData',
				elm$json$Json$Decode$string,
				A3(
					NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'searchFilterKey',
					elm$json$Json$Decode$string,
					A3(
						NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
						'index',
						elm$json$Json$Decode$int,
						elm$json$Json$Decode$succeed(author$project$Model$SearchFilterType)))))));
var author$project$Commands$truckTypeDecoder = A2(
	elm$json$Json$Decode$andThen,
	function (truckTypeValue) {
		switch (truckTypeValue) {
			case 'I':
				return elm$json$Json$Decode$succeed('Inventory');
			case 'A':
				return elm$json$Json$Decode$succeed('Appraisal');
			default:
				return elm$json$Json$Decode$succeed('Purchase Order');
		}
	},
	elm$json$Json$Decode$string);
var author$project$Model$Truck = function (id) {
	return function (name) {
		return function (stockNumber) {
			return function (appraisalNumber) {
				return function (poNumber) {
					return function (price) {
						return function (title) {
							return function (condition) {
								return function (make) {
									return function (model) {
										return function (engineMake) {
											return function (engineModel) {
												return function (engineHP) {
													return function (apu) {
														return function (cdl) {
															return function (year) {
																return function (primaryImageLink) {
																	return function (truckType) {
																		return function (suspension) {
																			return function (bodyType) {
																				return function (sleeperBunk) {
																					return function (salesStatus) {
																						return function (sleeperRoof) {
																							return function (rearAxleType) {
																								return function (sleeperInches) {
																									return function (wheelBase) {
																										return function (chassisNumber) {
																											return function (transType) {
																												return function (mileage) {
																													return function (frontAxleWeight) {
																														return function (rearAxleWeight) {
																															return function (fleetCode) {
																																return function (specialFinancing) {
																																	return function (inventoryAge) {
																																		return function (owningBranch) {
																																			return function (location) {
																																				return function (locationName) {
																																					return function (salesStatusFlag) {
																																						return function (hasPhoto) {
																																							return function (brakeType) {
																																								return function (rearAxleRatio) {
																																									return function (exhaustType) {
																																										return function (rearWheelSize) {
																																											return function (frontWheelSize) {
																																												return {bO: appraisalNumber, db: apu, dk: bodyType, ds: brakeType, du: cdl, dw: chassisNumber, dC: condition, dM: engineHP, dN: engineMake, dO: engineModel, dP: exhaustType, br: fleetCode, dZ: frontAxleWeight, d_: frontWheelSize, d1: hasPhoto, d5: id, ed: inventoryAge, eg: location, eh: locationName, ej: make, el: mileage, ep: model, eq: name, eC: owningBranch, eF: poNumber, bA: price, eG: primaryImageLink, eH: rearAxleRatio, eI: rearAxleType, eJ: rearAxleWeight, eK: rearWheelSize, eP: salesStatus, eQ: salesStatusFlag, e3: sleeperBunk, e4: sleeperInches, e5: sleeperRoof, e8: specialFinancing, cO: stockNumber, fh: suspension, fy: title, fB: transType, fF: truckType, fN: wheelBase, fP: year};
																																											};
																																										};
																																									};
																																								};
																																							};
																																						};
																																					};
																																				};
																																			};
																																		};
																																	};
																																};
																															};
																														};
																													};
																												};
																											};
																										};
																									};
																								};
																							};
																						};
																					};
																				};
																			};
																		};
																	};
																};
															};
														};
													};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var elm$json$Json$Decode$float = _Json_decodeFloat;
var author$project$Commands$trucksDecoder = A3(
	NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'frontWheelSize',
	elm$json$Json$Decode$float,
	A3(
		NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'rearWheelSize',
		elm$json$Json$Decode$float,
		A3(
			NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'exhaustType',
			elm$json$Json$Decode$string,
			A3(
				NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'rearAxleRatio',
				elm$json$Json$Decode$float,
				A3(
					NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'brakeType',
					elm$json$Json$Decode$string,
					A3(
						NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
						'hasPhoto',
						elm$json$Json$Decode$string,
						A3(
							NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
							'salesStatusFlag',
							elm$json$Json$Decode$string,
							A3(
								NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
								'locationName',
								elm$json$Json$Decode$string,
								A3(
									NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
									'locationNumber',
									elm$json$Json$Decode$string,
									A3(
										NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
										'owningBranch',
										elm$json$Json$Decode$string,
										A3(
											NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
											'inventoryAge',
											elm$json$Json$Decode$float,
											A3(
												NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
												'specialFinancing',
												elm$json$Json$Decode$string,
												A3(
													NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
													'fleetCode',
													elm$json$Json$Decode$string,
													A3(
														NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
														'rearAxleWeight',
														elm$json$Json$Decode$float,
														A3(
															NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
															'frontAxleWeight',
															elm$json$Json$Decode$float,
															A3(
																NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																'mileage',
																elm$json$Json$Decode$float,
																A3(
																	NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																	'transType',
																	elm$json$Json$Decode$string,
																	A3(
																		NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																		'chassisNumber',
																		elm$json$Json$Decode$string,
																		A3(
																			NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																			'wheelBase',
																			elm$json$Json$Decode$float,
																			A3(
																				NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																				'sleeperInches',
																				elm$json$Json$Decode$float,
																				A3(
																					NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																					'rearAxleType',
																					elm$json$Json$Decode$string,
																					A3(
																						NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																						'sleeperRoof',
																						elm$json$Json$Decode$string,
																						A3(
																							NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																							'salesStatus',
																							elm$json$Json$Decode$string,
																							A3(
																								NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																								'sleeperBunk',
																								elm$json$Json$Decode$string,
																								A3(
																									NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																									'bodyType',
																									elm$json$Json$Decode$string,
																									A3(
																										NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																										'suspension',
																										elm$json$Json$Decode$string,
																										A3(
																											NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																											'truckType',
																											author$project$Commands$truckTypeDecoder,
																											A3(
																												NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																												'primaryImageLink',
																												elm$json$Json$Decode$string,
																												A3(
																													NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																													'year',
																													elm$json$Json$Decode$string,
																													A3(
																														NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																														'cdl',
																														elm$json$Json$Decode$string,
																														A3(
																															NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																															'apu',
																															elm$json$Json$Decode$string,
																															A3(
																																NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																																'engineHP',
																																elm$json$Json$Decode$float,
																																A3(
																																	NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																																	'engineModel',
																																	elm$json$Json$Decode$string,
																																	A3(
																																		NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																																		'engineMake',
																																		elm$json$Json$Decode$string,
																																		A3(
																																			NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																																			'model',
																																			elm$json$Json$Decode$string,
																																			A3(
																																				NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																																				'make',
																																				elm$json$Json$Decode$string,
																																				A3(
																																					NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																																					'condition',
																																					elm$json$Json$Decode$string,
																																					A3(
																																						NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																																						'title',
																																						elm$json$Json$Decode$string,
																																						A3(
																																							NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																																							'price',
																																							elm$json$Json$Decode$float,
																																							A3(
																																								NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																																								'poNumber',
																																								elm$json$Json$Decode$string,
																																								A3(
																																									NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																																									'appraisalNumber',
																																									elm$json$Json$Decode$int,
																																									A3(
																																										NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																																										'stockNumber',
																																										elm$json$Json$Decode$int,
																																										A3(
																																											NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																																											'name',
																																											elm$json$Json$Decode$string,
																																											A3(
																																												NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																																												'id',
																																												elm$json$Json$Decode$string,
																																												elm$json$Json$Decode$succeed(author$project$Model$Truck)))))))))))))))))))))))))))))))))))))))))))));
var author$project$Model$TruckData = F5(
	function (pages, searchFilters, trucks, totalTrucksCount, cleanSearchFilterBullets) {
		return {dx: cleanSearchFilterBullets, eD: pages, eZ: searchFilters, bH: totalTrucksCount, fG: trucks};
	});
var elm$json$Json$Decode$bool = _Json_decodeBool;
var elm$json$Json$Decode$list = _Json_decodeList;
var author$project$Commands$fetchTrucksDecoder = A3(
	NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'cleanSearchFilterBullets',
	elm$json$Json$Decode$bool,
	A3(
		NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'totalTrucksCount',
		elm$json$Json$Decode$int,
		A3(
			NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'finalFilteredTrucks',
			elm$json$Json$Decode$list(author$project$Commands$trucksDecoder),
			A3(
				NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'searchFilters',
				elm$json$Json$Decode$list(author$project$Commands$searchFilterDecoder),
				A3(
					NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'pages',
					elm$json$Json$Decode$int,
					elm$json$Json$Decode$succeed(author$project$Model$TruckData))))));
var author$project$Msg$OnFetchTrucks = function (a) {
	return {$: 0, a: a};
};
var elm$core$Result$mapError = F2(
	function (f, result) {
		if (!result.$) {
			var v = result.a;
			return elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return elm$core$Result$Err(
				f(e));
		}
	});
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var elm$core$Basics$identity = function (x) {
	return x;
};
var elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var elm$core$Basics$compare = _Utils_compare;
var elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _n1 = A2(elm$core$Basics$compare, targetKey, key);
				switch (_n1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var elm$core$Dict$Black = 1;
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Dict$Red = 0;
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _n5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _n6 = left.d;
				var _n7 = _n6.a;
				var llK = _n6.b;
				var llV = _n6.c;
				var llLeft = _n6.d;
				var llRight = _n6.e;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5(elm$core$Dict$RBNode_elm_builtin, 0, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1) {
				case 0:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5(elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3(elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _n0 = A3(elm$core$Dict$insertHelp, key, value, dict);
		if ((_n0.$ === -1) && (!_n0.a)) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var lLeft = _n1.d;
			var lRight = _n1.e;
			var _n2 = dict.e;
			var rClr = _n2.a;
			var rK = _n2.b;
			var rV = _n2.c;
			var rLeft = _n2.d;
			var _n3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _n2.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5(elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n4 = dict.d;
			var lClr = _n4.a;
			var lK = _n4.b;
			var lV = _n4.c;
			var lLeft = _n4.d;
			var lRight = _n4.e;
			var _n5 = dict.e;
			var rClr = _n5.a;
			var rK = _n5.b;
			var rV = _n5.c;
			var rLeft = _n5.d;
			var rRight = _n5.e;
			if (clr === 1) {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var _n2 = _n1.d;
			var _n3 = _n2.a;
			var llK = _n2.b;
			var llV = _n2.c;
			var llLeft = _n2.d;
			var llRight = _n2.e;
			var lRight = _n1.e;
			var _n4 = dict.e;
			var rClr = _n4.a;
			var rK = _n4.b;
			var rV = _n4.c;
			var rLeft = _n4.d;
			var rRight = _n4.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5(elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n5 = dict.d;
			var lClr = _n5.a;
			var lK = _n5.b;
			var lV = _n5.c;
			var lLeft = _n5.d;
			var lRight = _n5.e;
			var _n6 = dict.e;
			var rClr = _n6.a;
			var rK = _n6.b;
			var rV = _n6.c;
			var rLeft = _n6.d;
			var rRight = _n6.e;
			if (clr === 1) {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _n1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5(elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_n2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _n3 = right.a;
							var _n4 = right.d;
							var _n5 = _n4.a;
							return elm$core$Dict$moveRedRight(dict);
						} else {
							break _n2$2;
						}
					} else {
						var _n6 = right.a;
						var _n7 = right.d;
						return elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _n2$2;
				}
			}
			return dict;
		}
	});
var elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _n3 = lLeft.a;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					elm$core$Dict$removeMin(left),
					right);
			} else {
				var _n4 = elm$core$Dict$moveRedLeft(dict);
				if (_n4.$ === -1) {
					var nColor = _n4.a;
					var nKey = _n4.b;
					var nValue = _n4.c;
					var nLeft = _n4.d;
					var nRight = _n4.e;
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _n4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _n6 = lLeft.a;
						return A5(
							elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2(elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _n7 = elm$core$Dict$moveRedLeft(dict);
						if (_n7.$ === -1) {
							var nColor = _n7.a;
							var nKey = _n7.b;
							var nValue = _n7.c;
							var nLeft = _n7.d;
							var nRight = _n7.e;
							return A5(
								elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2(elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2(elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7(elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _n1 = elm$core$Dict$getMin(right);
				if (_n1.$ === -1) {
					var minKey = _n1.b;
					var minValue = _n1.c;
					return A5(
						elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						elm$core$Dict$removeMin(right));
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2(elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var elm$core$Dict$remove = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$removeHelp, key, dict);
		if ((_n0.$ === -1) && (!_n0.a)) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _n0 = alter(
			A2(elm$core$Dict$get, targetKey, dictionary));
		if (!_n0.$) {
			var value = _n0.a;
			return A3(elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2(elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var elm$core$Maybe$isJust = function (maybe) {
	if (!maybe.$) {
		return true;
	} else {
		return false;
	}
};
var elm$core$Platform$sendToApp = _Platform_sendToApp;
var elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var elm$core$Result$map = F2(
	function (func, ra) {
		if (!ra.$) {
			var a = ra.a;
			return elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return elm$core$Result$Err(e);
		}
	});
var elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var elm$http$Http$BadUrl_ = function (a) {
	return {$: 0, a: a};
};
var elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var elm$http$Http$NetworkError_ = {$: 2};
var elm$http$Http$Receiving = function (a) {
	return {$: 1, a: a};
};
var elm$http$Http$Sending = function (a) {
	return {$: 0, a: a};
};
var elm$http$Http$Timeout_ = {$: 1};
var elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			elm$core$Basics$identity,
			A2(elm$core$Basics$composeR, toResult, toMsg));
	});
var elm$http$Http$BadBody = function (a) {
	return {$: 4, a: a};
};
var elm$http$Http$BadStatus = function (a) {
	return {$: 3, a: a};
};
var elm$http$Http$BadUrl = function (a) {
	return {$: 0, a: a};
};
var elm$http$Http$NetworkError = {$: 2};
var elm$http$Http$Timeout = {$: 1};
var elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 0:
				var url = response.a;
				return elm$core$Result$Err(
					elm$http$Http$BadUrl(url));
			case 1:
				return elm$core$Result$Err(elm$http$Http$Timeout);
			case 2:
				return elm$core$Result$Err(elm$http$Http$NetworkError);
			case 3:
				var metadata = response.a;
				return elm$core$Result$Err(
					elm$http$Http$BadStatus(metadata.cN));
			default:
				var body = response.b;
				return A2(
					elm$core$Result$mapError,
					elm$http$Http$BadBody,
					toResult(body));
		}
	});
var elm$json$Json$Decode$decodeString = _Json_runOnString;
var elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			elm$http$Http$expectStringResponse,
			toMsg,
			elm$http$Http$resolve(
				function (string) {
					return A2(
						elm$core$Result$mapError,
						elm$json$Json$Decode$errorToString,
						A2(elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var elm$http$Http$emptyBody = _Http_emptyBody;
var elm$http$Http$Request = function (a) {
	return {$: 1, a: a};
};
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$http$Http$State = F2(
	function (reqs, subs) {
		return {cC: reqs, cP: subs};
	});
var elm$http$Http$init = elm$core$Task$succeed(
	A2(elm$http$Http$State, elm$core$Dict$empty, _List_Nil));
var elm$core$Task$andThen = _Scheduler_andThen;
var elm$core$Process$kill = _Scheduler_kill;
var elm$core$Process$spawn = _Scheduler_spawn;
var elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (!cmd.$) {
					var tracker = cmd.a;
					var _n2 = A2(elm$core$Dict$get, tracker, reqs);
					if (_n2.$ === 1) {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _n2.a;
						return A2(
							elm$core$Task$andThen,
							function (_n3) {
								return A3(
									elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2(elm$core$Dict$remove, tracker, reqs));
							},
							elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						elm$core$Task$andThen,
						function (pid) {
							var _n4 = req.G;
							if (_n4.$ === 1) {
								return A3(elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _n4.a;
								return A3(
									elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3(elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			elm$core$Task$andThen,
			function (reqs) {
				return elm$core$Task$succeed(
					A2(elm$http$Http$State, reqs, subs));
			},
			A3(elm$http$Http$updateReqs, router, cmds, state.cC));
	});
var elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _n0 = f(mx);
		if (!_n0.$) {
			var x = _n0.a;
			return A2(elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return A2(
					elm$core$Task$andThen,
					function (b) {
						return elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var elm$core$Task$sequence = function (tasks) {
	return A3(
		elm$core$List$foldr,
		elm$core$Task$map2(elm$core$List$cons),
		elm$core$Task$succeed(_List_Nil),
		tasks);
};
var elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _n0) {
		var actualTracker = _n0.a;
		var toMsg = _n0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? elm$core$Maybe$Just(
			A2(
				elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : elm$core$Maybe$Nothing;
	});
var elm$http$Http$onSelfMsg = F3(
	function (router, _n0, state) {
		var tracker = _n0.a;
		var progress = _n0.b;
		return A2(
			elm$core$Task$andThen,
			function (_n1) {
				return elm$core$Task$succeed(state);
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$filterMap,
					A3(elm$http$Http$maybeSend, router, tracker, progress),
					state.cP)));
	});
var elm$http$Http$Cancel = function (a) {
	return {$: 0, a: a};
};
var elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (!cmd.$) {
			var tracker = cmd.a;
			return elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return elm$http$Http$Request(
				{
					ap: r.ap,
					dj: r.dj,
					dQ: A2(_Http_mapExpect, func, r.dQ),
					v: r.v,
					A: r.A,
					D: r.D,
					G: r.G,
					fJ: r.fJ
				});
		}
	});
var elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$http$Http$subMap = F2(
	function (func, _n0) {
		var tracker = _n0.a;
		var toMsg = _n0.b;
		return A2(
			elm$http$Http$MySub,
			tracker,
			A2(elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager(elm$http$Http$init, elm$http$Http$onEffects, elm$http$Http$onSelfMsg, elm$http$Http$cmdMap, elm$http$Http$subMap);
var elm$http$Http$command = _Platform_leaf('Http');
var elm$http$Http$subscription = _Platform_leaf('Http');
var elm$http$Http$request = function (r) {
	return elm$http$Http$command(
		elm$http$Http$Request(
			{ap: false, dj: r.dj, dQ: r.dQ, v: r.v, A: r.A, D: r.D, G: r.G, fJ: r.fJ}));
};
var elm$http$Http$get = function (r) {
	return elm$http$Http$request(
		{dj: elm$http$Http$emptyBody, dQ: r.dQ, v: _List_Nil, A: 'GET', D: elm$core$Maybe$Nothing, G: elm$core$Maybe$Nothing, fJ: r.fJ});
};
var elm$url$Url$Builder$toQueryPair = function (_n0) {
	var key = _n0.a;
	var value = _n0.b;
	return key + ('=' + value);
};
var elm$url$Url$Builder$toQuery = function (parameters) {
	if (!parameters.b) {
		return '';
	} else {
		return '?' + A2(
			elm$core$String$join,
			'&',
			A2(elm$core$List$map, elm$url$Url$Builder$toQueryPair, parameters));
	}
};
var elm$url$Url$Builder$crossOrigin = F3(
	function (prePath, pathSegments, parameters) {
		return prePath + ('/' + (A2(elm$core$String$join, '/', pathSegments) + elm$url$Url$Builder$toQuery(parameters)));
	});
var elm$url$Url$percentEncode = _Url_percentEncode;
var elm$url$Url$Builder$QueryParameter = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$url$Url$Builder$int = F2(
	function (key, value) {
		return A2(
			elm$url$Url$Builder$QueryParameter,
			elm$url$Url$percentEncode(key),
			elm$core$String$fromInt(value));
	});
var elm$url$Url$Builder$string = F2(
	function (key, value) {
		return A2(
			elm$url$Url$Builder$QueryParameter,
			elm$url$Url$percentEncode(key),
			elm$url$Url$percentEncode(value));
	});
var author$project$Commands$fetchTrucks = F5(
	function (searchFilterParam, searchText, pageNumber, sortField, sortOrder) {
		var url = A3(
			elm$url$Url$Builder$crossOrigin,
			'http://172.21.123.180/NewMHCtruckSyncapidemo/api/mhc/gettruckspaged',
			_List_Nil,
			_List_fromArray(
				[
					A2(elm$url$Url$Builder$string, 'filterString', searchFilterParam),
					A2(elm$url$Url$Builder$string, 'searchText', searchText),
					A2(elm$url$Url$Builder$int, 'pageNumber', pageNumber),
					A2(elm$url$Url$Builder$string, 'sortField', sortField),
					A2(elm$url$Url$Builder$string, 'sortOrder', sortOrder)
				]));
		return elm$http$Http$get(
			{
				dQ: A2(elm$http$Http$expectJson, author$project$Msg$OnFetchTrucks, author$project$Commands$fetchTrucksDecoder),
				fJ: url
			});
	});
var author$project$Model$MakeAtoZ = 4;
var author$project$Model$SortDESC = 1;
var author$project$Model$searchFiltersInitialExpandState = _List_fromArray(
	[
		{f: false, h: 0},
		{f: false, h: 13},
		{f: true, h: 1},
		{f: false, h: 12},
		{f: false, h: 14},
		{f: false, h: 2},
		{f: true, h: 3},
		{f: false, h: 4},
		{f: false, h: 5},
		{f: false, h: 6},
		{f: false, h: 7},
		{f: false, h: 8},
		{f: false, h: 9},
		{f: false, h: 11},
		{f: false, h: 29},
		{f: false, h: 15},
		{f: false, h: 10},
		{f: false, h: 16},
		{f: false, h: 17},
		{f: false, h: 18},
		{f: false, h: 19},
		{f: false, h: 20},
		{f: true, h: 21},
		{f: false, h: 23},
		{f: false, h: 22},
		{f: false, h: 24},
		{f: false, h: 26},
		{f: false, h: 27},
		{f: false, h: 28},
		{f: false, h: 25},
		{f: false, h: 30},
		{f: false, h: 31},
		{f: false, h: 32}
	]);
var elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, list);
			var jsArray = _n0.a;
			var remainingItems = _n0.b;
			if (_Utils_cmp(
				elm$core$Elm$JsArray$length(jsArray),
				elm$core$Array$branchFactor) < 0) {
				return A2(
					elm$core$Array$builderToArray,
					true,
					{q: nodeList, n: nodeListSize, p: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					elm$core$List$cons,
					elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return elm$core$Array$empty;
	} else {
		return A3(elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var author$project$Model$initalUIModel = function (jsFlag) {
	return {
		ao: _List_Nil,
		U: 1,
		aH: {Y: 'Make', Z: 4, _: 'Make A to Z', aa: 'ASC'},
		bV: 1,
		aJ: elm$core$Array$fromList(
			A2(
				elm$core$List$indexedMap,
				F2(
					function (index, searchFilterTypeRecord) {
						return {bv: index, eV: searchFilterTypeRecord.h, fK: searchFilterTypeRecord.f};
					}),
				author$project$Model$searchFiltersInitialExpandState)),
		b3: false,
		b4: false,
		ca: true,
		cn: A2(elm$core$String$split, '&', jsFlag),
		M: '',
		bD: _List_Nil,
		bE: false,
		O: false,
		bj: 1,
		fL: ''
	};
};
var author$project$Model$initialModel = {b_: _List_Nil, cq: _List_Nil, bH: 0, aC: _List_Nil};
var elm$core$Platform$Cmd$batch = _Platform_batch;
var author$project$Main$init = function (jsflg) {
	return _Utils_Tuple2(
		_Utils_Tuple2(
			author$project$Model$initialModel,
			author$project$Model$initalUIModel(jsflg)),
		elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					A5(author$project$Commands$fetchTrucks, '', '', 1, 'Make', 'ASC')
				])));
};
var elm$core$Platform$Sub$batch = _Platform_batch;
var elm$core$Platform$Sub$none = elm$core$Platform$Sub$batch(_List_Nil);
var author$project$Main$subscriptions = function (model) {
	return elm$core$Platform$Sub$none;
};
var elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2(elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var elm$core$String$length = _String_length;
var author$project$BusinessFunctions$SearchFilterFunctions$formattedSelectedFilterBullets = function (uiModel) {
	return (elm$core$List$length(uiModel.bD) > 0) ? A2(
		elm$core$List$map,
		function (eachFilterType) {
			return function (finalStr) {
				return (elm$core$String$length(finalStr) > 0) ? (eachFilterType.g + ('=' + finalStr)) : '';
			}(
				A2(
					elm$core$String$join,
					',',
					A2(
						elm$core$List$map,
						function (eachBullet) {
							return eachBullet.eX;
						},
						A2(
							elm$core$List$filter,
							function (selectedFilterBullet) {
								return _Utils_eq(selectedFilterBullet.dV, eachFilterType.h);
							},
							uiModel.bD))));
		},
		author$project$BusinessFunctions$SearchFilterFunctions$partialSearchFiltersMetadata) : _List_Nil;
};
var author$project$Msg$NOoP = {$: 14};
var elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var elm$browser$Browser$Dom$NotFound = elm$core$Basics$identity;
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var elm$core$Task$Perform = elm$core$Basics$identity;
var elm$core$Task$init = elm$core$Task$succeed(0);
var elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var elm$core$Task$spawnCmd = F2(
	function (router, _n0) {
		var task = _n0;
		return _Scheduler_spawn(
			A2(
				elm$core$Task$andThen,
				elm$core$Platform$sendToApp(router),
				task));
	});
var elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			elm$core$Task$map,
			function (_n0) {
				return 0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0;
		return A2(elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			A2(elm$core$Task$map, toMessage, task));
	});
var elm$json$Json$Decode$map = _Json_map1;
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var elm$core$String$slice = _String_slice;
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var elm$core$String$startsWith = _String_startsWith;
var elm$url$Url$Http = 0;
var elm$url$Url$Https = 1;
var elm$core$String$indexes = _String_indexes;
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var elm$core$String$contains = _String_contains;
var elm$core$String$toInt = _String_toInt;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {b0: fragment, b8: host, cs: path, cv: port_, cz: protocol, cA: query};
	});
var elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if (elm$core$String$isEmpty(str) || A2(elm$core$String$contains, '@', str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, ':', str);
			if (!_n0.b) {
				return elm$core$Maybe$Just(
					A6(elm$url$Url$Url, protocol, str, elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_n0.b.b) {
					var i = _n0.a;
					var _n1 = elm$core$String$toInt(
						A2(elm$core$String$dropLeft, i + 1, str));
					if (_n1.$ === 1) {
						return elm$core$Maybe$Nothing;
					} else {
						var port_ = _n1;
						return elm$core$Maybe$Just(
							A6(
								elm$url$Url$Url,
								protocol,
								A2(elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}
	});
var elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '/', str);
			if (!_n0.b) {
				return A5(elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _n0.a;
				return A5(
					elm$url$Url$chompBeforePath,
					protocol,
					A2(elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '?', str);
			if (!_n0.b) {
				return A4(elm$url$Url$chompBeforeQuery, protocol, elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _n0.a;
				return A4(
					elm$url$Url$chompBeforeQuery,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '#', str);
			if (!_n0.b) {
				return A3(elm$url$Url$chompBeforeFragment, protocol, elm$core$Maybe$Nothing, str);
			} else {
				var i = _n0.a;
				return A3(
					elm$url$Url$chompBeforeFragment,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$fromString = function (str) {
	return A2(elm$core$String$startsWith, 'http://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		0,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		1,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$Dom$setViewport = _Browser_setViewport;
var elm$core$Elm$JsArray$map = _JsArray_map;
var elm$core$Array$map = F2(
	function (func, _n0) {
		var len = _n0.a;
		var startShift = _n0.b;
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = function (node) {
			if (!node.$) {
				var subTree = node.a;
				return elm$core$Array$SubTree(
					A2(elm$core$Elm$JsArray$map, helper, subTree));
			} else {
				var values = node.a;
				return elm$core$Array$Leaf(
					A2(elm$core$Elm$JsArray$map, func, values));
			}
		};
		return A4(
			elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A2(elm$core$Elm$JsArray$map, helper, tree),
			A2(elm$core$Elm$JsArray$map, func, tail));
	});
var elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var elm$core$Array$bitMask = 4294967295 >>> (32 - elm$core$Array$shiftStep);
var elm$core$Bitwise$and = _Bitwise_and;
var elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var elm$core$Array$setHelp = F4(
	function (shift, index, value, tree) {
		var pos = elm$core$Array$bitMask & (index >>> shift);
		var _n0 = A2(elm$core$Elm$JsArray$unsafeGet, pos, tree);
		if (!_n0.$) {
			var subTree = _n0.a;
			var newSub = A4(elm$core$Array$setHelp, shift - elm$core$Array$shiftStep, index, value, subTree);
			return A3(
				elm$core$Elm$JsArray$unsafeSet,
				pos,
				elm$core$Array$SubTree(newSub),
				tree);
		} else {
			var values = _n0.a;
			var newLeaf = A3(elm$core$Elm$JsArray$unsafeSet, elm$core$Array$bitMask & index, value, values);
			return A3(
				elm$core$Elm$JsArray$unsafeSet,
				pos,
				elm$core$Array$Leaf(newLeaf),
				tree);
		}
	});
var elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var elm$core$Basics$ge = _Utils_ge;
var elm$core$Array$set = F3(
	function (index, value, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? array : ((_Utils_cmp(
			index,
			elm$core$Array$tailIndex(len)) > -1) ? A4(
			elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			tree,
			A3(elm$core$Elm$JsArray$unsafeSet, elm$core$Array$bitMask & index, value, tail)) : A4(
			elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A4(elm$core$Array$setHelp, startShift, index, value, tree),
			tail));
	});
var elm$core$Basics$neq = _Utils_notEqual;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var elm_community$list_extra$List$Extra$remove = F2(
	function (x, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var y = xs.a;
			var ys = xs.b;
			return _Utils_eq(x, y) ? ys : A2(
				elm$core$List$cons,
				y,
				A2(elm_community$list_extra$List$Extra$remove, x, ys));
		}
	});
var author$project$Main$update = F2(
	function (msg, _n0) {
		var model = _n0.a;
		var uiModel = _n0.b;
		var getTrucksHttpCmd = function (newUIModel) {
			var searchFilterString = A2(
				elm$core$String$join,
				'|',
				A2(
					elm$core$List$filter,
					function (sb) {
						return sb !== '';
					},
					author$project$BusinessFunctions$SearchFilterFunctions$formattedSelectedFilterBullets(newUIModel)));
			return A5(author$project$Commands$fetchTrucks, searchFilterString, newUIModel.M, newUIModel.U, newUIModel.aH.Y, newUIModel.aH.aa);
		};
		var executeTextSearch = function () {
			var newUIModel = _Utils_update(
				uiModel,
				{U: 1, O: true});
			return _Utils_Tuple2(
				_Utils_Tuple2(model, newUIModel),
				getTrucksHttpCmd(uiModel));
		}();
		switch (msg.$) {
			case 0:
				var response = msg.a;
				var _n2 = function () {
					if (!response.$) {
						var rangeFltrs = response.a;
						return rangeFltrs;
					} else {
						var err = response.a;
						return A5(author$project$Model$TruckData, 0, _List_Nil, _List_Nil, 0, true);
					}
				}();
				var pages = _n2.eD;
				var searchFilters = _n2.eZ;
				var trucks = _n2.fG;
				var totalTrucksCount = _n2.bH;
				var cleanSearchFilterBullets = _n2.dx;
				var updatedUIModel = _Utils_update(
					uiModel,
					{
						ao: searchFilters,
						ca: false,
						bD: cleanSearchFilterBullets ? _List_Nil : uiModel.bD,
						O: false,
						bj: pages
					});
				var newModel = _Utils_update(
					model,
					{bH: totalTrucksCount, aC: trucks});
				return _Utils_Tuple2(
					_Utils_Tuple2(newModel, updatedUIModel),
					elm$core$Platform$Cmd$none);
			case 1:
				var selectedSearchFilter = msg.a;
				var userAction = msg.b;
				var updateUserSelectedSearchFilter = function (filterList) {
					var abc = A2(
						elm_community$list_extra$List$Extra$find,
						function (sf) {
							return _Utils_eq(sf.al, selectedSearchFilter.al) && _Utils_eq(sf.dV, selectedSearchFilter.dV);
						},
						elm$core$Array$toList(filterList));
					return A2(
						elm$core$Maybe$withDefault,
						uiModel,
						A2(
							elm$core$Maybe$map,
							function (fltrs) {
								return _Utils_update(
									uiModel,
									{
										ao: elm$core$Array$toList(fltrs)
									});
							},
							A2(
								elm$core$Maybe$map,
								function (sf) {
									return A3(elm$core$Array$set, sf.bv, sf, filterList);
								},
								A2(
									elm$core$Maybe$map,
									function (sf) {
										return _Utils_update(
											sf,
											{fK: userAction});
									},
									A2(
										elm_community$list_extra$List$Extra$find,
										function (sf) {
											return _Utils_eq(sf.al, selectedSearchFilter.al) && _Utils_eq(sf.dV, selectedSearchFilter.dV);
										},
										elm$core$Array$toList(filterList))))));
				};
				var newUIModel = updateUserSelectedSearchFilter(
					elm$core$Array$fromList(uiModel.ao));
				var newUIModelUpdatedWithSearchFilterBullets = _Utils_update(
					newUIModel,
					{
						U: 1,
						bD: function () {
							if (userAction) {
								var selectedFilterBullet = A6(author$project$Model$SearchFilterType, selectedSearchFilter.bv, selectedSearchFilter.al, selectedSearchFilter.eX, userAction, 0, selectedSearchFilter.dV);
								return A2(elm$core$List$cons, selectedFilterBullet, newUIModel.bD);
							} else {
								return A2(
									elm$core$Maybe$withDefault,
									newUIModel.bD,
									A2(
										elm$core$Maybe$map,
										function (sf) {
											return A2(elm_community$list_extra$List$Extra$remove, sf, newUIModel.bD);
										},
										A2(
											elm_community$list_extra$List$Extra$find,
											function (sf) {
												return _Utils_eq(sf.al, selectedSearchFilter.al) && _Utils_eq(sf.dV, selectedSearchFilter.dV);
											},
											newUIModel.bD)));
							}
						}(),
						O: true
					});
				var newSortedFilteredTruckList = model.aC;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						_Utils_update(
							model,
							{aC: newSortedFilteredTruckList}),
						newUIModelUpdatedWithSearchFilterBullets),
					elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								getTrucksHttpCmd(newUIModelUpdatedWithSearchFilterBullets),
								A2(
								elm$core$Task$perform,
								function (_n4) {
									return author$project$Msg$NOoP;
								},
								A2(elm$browser$Browser$Dom$setViewport, 0, 0))
							])));
			case 14:
				return _Utils_Tuple2(
					_Utils_Tuple2(model, uiModel),
					elm$core$Platform$Cmd$none);
			case 2:
				var searchString = msg.a;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						model,
						_Utils_update(
							uiModel,
							{M: searchString})),
					elm$core$Platform$Cmd$none);
			case 3:
				return executeTextSearch;
			case 4:
				return executeTextSearch;
			case 5:
				var searchFilterState = msg.a;
				var userAction = msg.b;
				var newSearchFilterState = _Utils_update(
					searchFilterState,
					{fK: userAction});
				var updatedSearchFilterStates = A3(elm$core$Array$set, searchFilterState.bv, newSearchFilterState, uiModel.aJ);
				return _Utils_Tuple2(
					_Utils_Tuple2(
						model,
						_Utils_update(
							uiModel,
							{aJ: updatedSearchFilterStates})),
					elm$core$Platform$Cmd$none);
			case 6:
				var userAction = msg.a;
				var updatedSearchFilterStates = A2(
					elm$core$Array$map,
					function (item) {
						return _Utils_update(
							item,
							{fK: userAction});
					},
					uiModel.aJ);
				return _Utils_Tuple2(
					_Utils_Tuple2(
						model,
						_Utils_update(
							uiModel,
							{aJ: updatedSearchFilterStates})),
					elm$core$Platform$Cmd$none);
			case 7:
				var pageNumber = msg.a;
				var newUIModel = _Utils_update(
					uiModel,
					{U: pageNumber, O: true});
				return _Utils_Tuple2(
					_Utils_Tuple2(model, newUIModel),
					getTrucksHttpCmd(newUIModel));
			case 8:
				var show = msg.a;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						model,
						_Utils_update(
							uiModel,
							{bE: show})),
					elm$core$Platform$Cmd$none);
			case 9:
				var userAction = msg.a;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						model,
						_Utils_update(
							uiModel,
							{b4: userAction})),
					elm$core$Platform$Cmd$none);
			case 10:
				var selectedSortByMetaData = msg.a;
				var newUIModel = _Utils_update(
					uiModel,
					{U: 1, aH: selectedSortByMetaData, O: true});
				return _Utils_Tuple2(
					_Utils_Tuple2(model, newUIModel),
					getTrucksHttpCmd(newUIModel));
			case 11:
				var userAction = msg.a;
				return _Utils_Tuple2(
					_Utils_Tuple2(
						model,
						_Utils_update(
							uiModel,
							{O: userAction})),
					elm$core$Platform$Cmd$none);
			case 12:
				var newSearchFilters = A2(
					elm$core$List$map,
					function (sf) {
						return _Utils_update(
							sf,
							{fK: false});
					},
					uiModel.ao);
				var newUIModel = _Utils_update(
					uiModel,
					{ao: newSearchFilters, U: 1, M: '', bD: _List_Nil, O: true, bj: 0});
				return _Utils_Tuple2(
					_Utils_Tuple2(model, newUIModel),
					getTrucksHttpCmd(newUIModel));
			default:
				var newUIModel = _Utils_update(
					uiModel,
					{U: 1, M: '', O: true, bj: 0});
				return _Utils_Tuple2(
					_Utils_Tuple2(model, newUIModel),
					getTrucksHttpCmd(newUIModel));
		}
	});
var mdgriffith$elm_ui$Internal$Model$Rgba = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var mdgriffith$elm_ui$Element$rgb255 = F3(
	function (red, green, blue) {
		return A4(mdgriffith$elm_ui$Internal$Model$Rgba, red / 255, green / 255, blue / 255, 1);
	});
var mdgriffith$elm_ui$Internal$Flag$Flag = function (a) {
	return {$: 0, a: a};
};
var mdgriffith$elm_ui$Internal$Flag$Second = function (a) {
	return {$: 1, a: a};
};
var mdgriffith$elm_ui$Internal$Flag$flag = function (i) {
	return (i > 31) ? mdgriffith$elm_ui$Internal$Flag$Second(1 << (i - 32)) : mdgriffith$elm_ui$Internal$Flag$Flag(1 << i);
};
var mdgriffith$elm_ui$Internal$Flag$bgColor = mdgriffith$elm_ui$Internal$Flag$flag(8);
var mdgriffith$elm_ui$Internal$Model$Colored = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var mdgriffith$elm_ui$Internal$Model$StyleClass = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var elm$core$Basics$round = _Basics_round;
var mdgriffith$elm_ui$Internal$Model$floatClass = function (x) {
	return elm$core$String$fromInt(
		elm$core$Basics$round(x * 255));
};
var mdgriffith$elm_ui$Internal$Model$formatColorClass = function (_n0) {
	var red = _n0.a;
	var green = _n0.b;
	var blue = _n0.c;
	var alpha = _n0.d;
	return mdgriffith$elm_ui$Internal$Model$floatClass(red) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(green) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(blue) + ('-' + mdgriffith$elm_ui$Internal$Model$floatClass(alpha))))));
};
var mdgriffith$elm_ui$Element$Background$color = function (clr) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$bgColor,
		A3(
			mdgriffith$elm_ui$Internal$Model$Colored,
			'bg-' + mdgriffith$elm_ui$Internal$Model$formatColorClass(clr),
			'background-color',
			clr));
};
var author$project$Helpers$ElmStyleShotcuts$bc = F3(
	function (red, green, blue) {
		return mdgriffith$elm_ui$Element$Background$color(
			A3(mdgriffith$elm_ui$Element$rgb255, red, green, blue));
	});
var author$project$Helpers$Colors$greyBg = function (num) {
	return A3(author$project$Helpers$ElmStyleShotcuts$bc, num, num, num);
};
var mdgriffith$elm_ui$Internal$Flag$fontColor = mdgriffith$elm_ui$Internal$Flag$flag(14);
var mdgriffith$elm_ui$Element$Font$color = function (fontColor) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$fontColor,
		A3(
			mdgriffith$elm_ui$Internal$Model$Colored,
			'fc-' + mdgriffith$elm_ui$Internal$Model$formatColorClass(fontColor),
			'color',
			fontColor));
};
var author$project$Helpers$ElmStyleShotcuts$fc = F3(
	function (red, green, blue) {
		return mdgriffith$elm_ui$Element$Font$color(
			A3(mdgriffith$elm_ui$Element$rgb255, red, green, blue));
	});
var author$project$Helpers$Colors$greyFont = function (num) {
	return A3(author$project$Helpers$ElmStyleShotcuts$fc, num, num, num);
};
var author$project$Helpers$Colors$mhcMediumRed = A3(author$project$Helpers$ElmStyleShotcuts$fc, 244, 66, 95);
var mdgriffith$elm_ui$Internal$Model$AlignX = function (a) {
	return {$: 6, a: a};
};
var mdgriffith$elm_ui$Internal$Model$CenterX = 1;
var mdgriffith$elm_ui$Element$centerX = mdgriffith$elm_ui$Internal$Model$AlignX(1);
var author$project$Helpers$ElmStyleShotcuts$eacx = mdgriffith$elm_ui$Element$centerX;
var mdgriffith$elm_ui$Internal$Model$AlignY = function (a) {
	return {$: 5, a: a};
};
var mdgriffith$elm_ui$Internal$Model$CenterY = 1;
var mdgriffith$elm_ui$Element$centerY = mdgriffith$elm_ui$Internal$Model$AlignY(1);
var author$project$Helpers$ElmStyleShotcuts$eacy = mdgriffith$elm_ui$Element$centerY;
var mdgriffith$elm_ui$Internal$Flag$fontWeight = mdgriffith$elm_ui$Internal$Flag$flag(13);
var mdgriffith$elm_ui$Internal$Model$Class = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Style$classes = {c_: 'a', bm: 'atv', c1: 'ab', c2: 'cx', c3: 'cy', c4: 'acb', c5: 'accx', c6: 'accy', c7: 'acr', bM: 'al', bN: 'ar', c8: 'at', bn: 'ah', bo: 'av', da: 's', df: 'bh', dg: 'b', dl: 'w7', dn: 'bd', $7: 'bdt', aV: 'bn', dp: 'bs', aX: 'cpe', dy: 'cp', dz: 'cpx', dA: 'cpy', S: 'c', aZ: 'ctr', a_: 'cb', a$: 'ccx', T: 'ccy', aG: 'cl', a0: 'cr', dD: 'ct', dG: 'cptr', dH: 'ctxt', dY: 'fcs', d$: 'fs', d0: 'g', bt: 'hbh', a4: 'hc', bu: 'hf', b6: 'hfp', d3: 'hv', d6: 'ic', d8: 'fr', ea: 'iml', eb: 'it', ee: 'i', au: 'nb', ck: 'notxt', et: 'ol', ev: 'or', aj: 'oq', eB: 'oh', cp: 'pg', cr: 'p', eE: 'ppe', eN: 'ui', cH: 'r', eR: 'sb', eS: 'sbx', eT: 'sby', eU: 'sbt', e0: 'e', e2: 'cap', e6: 'sev', fd: 'sk', cR: 't', fi: 'tc', fj: 'w8', fk: 'w2', fl: 'w9', fm: 'tj', bi: 'tja', fn: 'tl', fo: 'w3', fp: 'w5', fq: 'w4', fr: 'tr', fs: 'w6', ft: 'w1', fu: 'tun', cT: 'ts', am: 'clr', fH: 'u', bI: 'wc', cX: 'we', bJ: 'wf', cY: 'wfp', bK: 'wrp'};
var mdgriffith$elm_ui$Element$Font$bold = A2(mdgriffith$elm_ui$Internal$Model$Class, mdgriffith$elm_ui$Internal$Flag$fontWeight, mdgriffith$elm_ui$Internal$Style$classes.dl);
var author$project$Helpers$ElmStyleShotcuts$fb = mdgriffith$elm_ui$Element$Font$bold;
var mdgriffith$elm_ui$Internal$Flag$fontSize = mdgriffith$elm_ui$Internal$Flag$flag(4);
var mdgriffith$elm_ui$Internal$Model$FontSize = function (a) {
	return {$: 2, a: a};
};
var mdgriffith$elm_ui$Element$Font$size = function (i) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$fontSize,
		mdgriffith$elm_ui$Internal$Model$FontSize(i));
};
var author$project$Helpers$ElmStyleShotcuts$fs = function (num) {
	return mdgriffith$elm_ui$Element$Font$size(num);
};
var mdgriffith$elm_ui$Internal$Model$Fill = function (a) {
	return {$: 2, a: a};
};
var mdgriffith$elm_ui$Element$fill = mdgriffith$elm_ui$Internal$Model$Fill(1);
var mdgriffith$elm_ui$Internal$Model$Height = function (a) {
	return {$: 8, a: a};
};
var mdgriffith$elm_ui$Element$height = mdgriffith$elm_ui$Internal$Model$Height;
var author$project$Helpers$ElmStyleShotcuts$hf = mdgriffith$elm_ui$Element$height(mdgriffith$elm_ui$Element$fill);
var mdgriffith$elm_ui$Internal$Model$Px = function (a) {
	return {$: 0, a: a};
};
var mdgriffith$elm_ui$Element$px = mdgriffith$elm_ui$Internal$Model$Px;
var author$project$Helpers$ElmStyleShotcuts$hpx = function (number) {
	return mdgriffith$elm_ui$Element$height(
		mdgriffith$elm_ui$Element$px(number));
};
var mdgriffith$elm_ui$Internal$Model$Width = function (a) {
	return {$: 7, a: a};
};
var mdgriffith$elm_ui$Element$width = mdgriffith$elm_ui$Internal$Model$Width;
var author$project$Helpers$ElmStyleShotcuts$wf = mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$fill);
var author$project$Helpers$ElmStyleShotcuts$wpx = function (number) {
	return mdgriffith$elm_ui$Element$width(
		mdgriffith$elm_ui$Element$px(number));
};
var mdgriffith$elm_ui$Internal$Model$Text = function (a) {
	return {$: 2, a: a};
};
var mdgriffith$elm_ui$Element$text = function (content) {
	return mdgriffith$elm_ui$Internal$Model$Text(content);
};
var author$project$Helpers$ElmUI$textValue = function (stringValue) {
	return mdgriffith$elm_ui$Element$text(stringValue);
};
var author$project$Msg$PageNumberClicked = function (a) {
	return {$: 7, a: a};
};
var mdgriffith$elm_ui$Internal$Model$Content = {$: 1};
var mdgriffith$elm_ui$Element$shrink = mdgriffith$elm_ui$Internal$Model$Content;
var mdgriffith$elm_ui$Internal$Model$Unkeyed = function (a) {
	return {$: 0, a: a};
};
var mdgriffith$elm_ui$Internal$Model$AsEl = 2;
var mdgriffith$elm_ui$Internal$Model$asEl = 2;
var mdgriffith$elm_ui$Internal$Model$Generic = {$: 0};
var mdgriffith$elm_ui$Internal$Model$div = mdgriffith$elm_ui$Internal$Model$Generic;
var mdgriffith$elm_ui$Internal$Flag$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Flag$none = A2(mdgriffith$elm_ui$Internal$Flag$Field, 0, 0);
var mdgriffith$elm_ui$Internal$Model$NoNearbyChildren = {$: 0};
var mdgriffith$elm_ui$Internal$Model$columnClass = mdgriffith$elm_ui$Internal$Style$classes.da + (' ' + mdgriffith$elm_ui$Internal$Style$classes.S);
var mdgriffith$elm_ui$Internal$Model$gridClass = mdgriffith$elm_ui$Internal$Style$classes.da + (' ' + mdgriffith$elm_ui$Internal$Style$classes.d0);
var mdgriffith$elm_ui$Internal$Model$pageClass = mdgriffith$elm_ui$Internal$Style$classes.da + (' ' + mdgriffith$elm_ui$Internal$Style$classes.cp);
var mdgriffith$elm_ui$Internal$Model$paragraphClass = mdgriffith$elm_ui$Internal$Style$classes.da + (' ' + mdgriffith$elm_ui$Internal$Style$classes.cr);
var mdgriffith$elm_ui$Internal$Model$rowClass = mdgriffith$elm_ui$Internal$Style$classes.da + (' ' + mdgriffith$elm_ui$Internal$Style$classes.cH);
var mdgriffith$elm_ui$Internal$Model$singleClass = mdgriffith$elm_ui$Internal$Style$classes.da + (' ' + mdgriffith$elm_ui$Internal$Style$classes.e0);
var mdgriffith$elm_ui$Internal$Model$contextClasses = function (context) {
	switch (context) {
		case 0:
			return mdgriffith$elm_ui$Internal$Model$rowClass;
		case 1:
			return mdgriffith$elm_ui$Internal$Model$columnClass;
		case 2:
			return mdgriffith$elm_ui$Internal$Model$singleClass;
		case 3:
			return mdgriffith$elm_ui$Internal$Model$gridClass;
		case 4:
			return mdgriffith$elm_ui$Internal$Model$paragraphClass;
		default:
			return mdgriffith$elm_ui$Internal$Model$pageClass;
	}
};
var elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var mdgriffith$elm_ui$Internal$Model$Keyed = function (a) {
	return {$: 1, a: a};
};
var mdgriffith$elm_ui$Internal$Model$NoStyleSheet = {$: 0};
var mdgriffith$elm_ui$Internal$Model$Styled = function (a) {
	return {$: 1, a: a};
};
var mdgriffith$elm_ui$Internal$Model$Unstyled = function (a) {
	return {$: 0, a: a};
};
var mdgriffith$elm_ui$Internal$Model$addChildren = F2(
	function (existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 0:
				return existing;
			case 1:
				var behind = nearbyChildren.a;
				return _Utils_ap(behind, existing);
			case 2:
				var inFront = nearbyChildren.a;
				return _Utils_ap(existing, inFront);
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					behind,
					_Utils_ap(existing, inFront));
		}
	});
var mdgriffith$elm_ui$Internal$Model$addKeyedChildren = F3(
	function (key, existing, nearbyChildren) {
		switch (nearbyChildren.$) {
			case 0:
				return existing;
			case 1:
				var behind = nearbyChildren.a;
				return _Utils_ap(
					A2(
						elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					existing);
			case 2:
				var inFront = nearbyChildren.a;
				return _Utils_ap(
					existing,
					A2(
						elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						inFront));
			default:
				var behind = nearbyChildren.a;
				var inFront = nearbyChildren.b;
				return _Utils_ap(
					A2(
						elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(key, x);
						},
						behind),
					_Utils_ap(
						existing,
						A2(
							elm$core$List$map,
							function (x) {
								return _Utils_Tuple2(key, x);
							},
							inFront)));
		}
	});
var mdgriffith$elm_ui$Internal$Model$AsParagraph = 4;
var mdgriffith$elm_ui$Internal$Model$asParagraph = 4;
var elm$core$Basics$not = _Basics_not;
var elm$html$Html$div = _VirtualDom_node('div');
var elm$html$Html$p = _VirtualDom_node('p');
var elm$html$Html$s = _VirtualDom_node('s');
var elm$html$Html$u = _VirtualDom_node('u');
var elm$json$Json$Encode$string = _Json_wrap;
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$class = elm$html$Html$Attributes$stringProperty('className');
var elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var mdgriffith$elm_ui$Internal$Flag$alignBottom = mdgriffith$elm_ui$Internal$Flag$flag(41);
var mdgriffith$elm_ui$Internal$Flag$alignRight = mdgriffith$elm_ui$Internal$Flag$flag(40);
var mdgriffith$elm_ui$Internal$Flag$centerX = mdgriffith$elm_ui$Internal$Flag$flag(42);
var mdgriffith$elm_ui$Internal$Flag$centerY = mdgriffith$elm_ui$Internal$Flag$flag(43);
var mdgriffith$elm_ui$Internal$Flag$heightBetween = mdgriffith$elm_ui$Internal$Flag$flag(45);
var mdgriffith$elm_ui$Internal$Flag$heightFill = mdgriffith$elm_ui$Internal$Flag$flag(37);
var mdgriffith$elm_ui$Internal$Flag$present = F2(
	function (myFlag, _n0) {
		var fieldOne = _n0.a;
		var fieldTwo = _n0.b;
		if (!myFlag.$) {
			var first = myFlag.a;
			return _Utils_eq(first & fieldOne, first);
		} else {
			var second = myFlag.a;
			return _Utils_eq(second & fieldTwo, second);
		}
	});
var mdgriffith$elm_ui$Internal$Flag$widthBetween = mdgriffith$elm_ui$Internal$Flag$flag(44);
var mdgriffith$elm_ui$Internal$Flag$widthFill = mdgriffith$elm_ui$Internal$Flag$flag(39);
var elm$core$Set$Set_elm_builtin = elm$core$Basics$identity;
var elm$core$Set$empty = elm$core$Dict$empty;
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var elm$core$Set$insert = F2(
	function (key, _n0) {
		var dict = _n0;
		return A3(elm$core$Dict$insert, key, 0, dict);
	});
var elm$core$Dict$member = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$get, key, dict);
		if (!_n0.$) {
			return true;
		} else {
			return false;
		}
	});
var elm$core$Set$member = F2(
	function (key, _n0) {
		var dict = _n0;
		return A2(elm$core$Dict$member, key, dict);
	});
var mdgriffith$elm_ui$Internal$Model$lengthClassName = function (x) {
	switch (x.$) {
		case 0:
			var px = x.a;
			return elm$core$String$fromInt(px) + 'px';
		case 1:
			return 'auto';
		case 2:
			var i = x.a;
			return elm$core$String$fromInt(i) + 'fr';
		case 3:
			var min = x.a;
			var len = x.b;
			return 'min' + (elm$core$String$fromInt(min) + mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
		default:
			var max = x.a;
			var len = x.b;
			return 'max' + (elm$core$String$fromInt(max) + mdgriffith$elm_ui$Internal$Model$lengthClassName(len));
	}
};
var mdgriffith$elm_ui$Internal$Model$transformClass = function (transform) {
	switch (transform.$) {
		case 0:
			return elm$core$Maybe$Nothing;
		case 1:
			var _n1 = transform.a;
			var x = _n1.a;
			var y = _n1.b;
			var z = _n1.c;
			return elm$core$Maybe$Just(
				'mv-' + (mdgriffith$elm_ui$Internal$Model$floatClass(x) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(y) + ('-' + mdgriffith$elm_ui$Internal$Model$floatClass(z))))));
		default:
			var _n2 = transform.a;
			var tx = _n2.a;
			var ty = _n2.b;
			var tz = _n2.c;
			var _n3 = transform.b;
			var sx = _n3.a;
			var sy = _n3.b;
			var sz = _n3.c;
			var _n4 = transform.c;
			var ox = _n4.a;
			var oy = _n4.b;
			var oz = _n4.c;
			var angle = transform.d;
			return elm$core$Maybe$Just(
				'tfrm-' + (mdgriffith$elm_ui$Internal$Model$floatClass(tx) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(ty) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(tz) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(sx) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(sy) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(sz) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(ox) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(oy) + ('-' + (mdgriffith$elm_ui$Internal$Model$floatClass(oz) + ('-' + mdgriffith$elm_ui$Internal$Model$floatClass(angle))))))))))))))))))));
	}
};
var mdgriffith$elm_ui$Internal$Model$getStyleName = function (style) {
	switch (style.$) {
		case 13:
			var name = style.a;
			return name;
		case 12:
			var name = style.a;
			var o = style.b;
			return name;
		case 0:
			var _class = style.a;
			return _class;
		case 1:
			var name = style.a;
			return name;
		case 2:
			var i = style.a;
			return 'font-size-' + elm$core$String$fromInt(i);
		case 3:
			var _class = style.a;
			return _class;
		case 4:
			var _class = style.a;
			return _class;
		case 5:
			var cls = style.a;
			var x = style.b;
			var y = style.c;
			return cls;
		case 7:
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 6:
			var cls = style.a;
			var top = style.b;
			var right = style.c;
			var bottom = style.d;
			var left = style.e;
			return cls;
		case 8:
			var template = style.a;
			return 'grid-rows-' + (A2(
				elm$core$String$join,
				'-',
				A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$lengthClassName, template.eO)) + ('-cols-' + (A2(
				elm$core$String$join,
				'-',
				A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$lengthClassName, template.J)) + ('-space-x-' + (mdgriffith$elm_ui$Internal$Model$lengthClassName(template.e7.a) + ('-space-y-' + mdgriffith$elm_ui$Internal$Model$lengthClassName(template.e7.b)))))));
		case 9:
			var pos = style.a;
			return 'gp grid-pos-' + (elm$core$String$fromInt(pos.cH) + ('-' + (elm$core$String$fromInt(pos.bU) + ('-' + (elm$core$String$fromInt(pos.aE) + ('-' + elm$core$String$fromInt(pos.as)))))));
		case 11:
			var selector = style.a;
			var subStyle = style.b;
			var name = function () {
				switch (selector) {
					case 0:
						return 'fs';
					case 1:
						return 'hv';
					default:
						return 'act';
				}
			}();
			return A2(
				elm$core$String$join,
				' ',
				A2(
					elm$core$List$map,
					function (sty) {
						var _n1 = mdgriffith$elm_ui$Internal$Model$getStyleName(sty);
						if (_n1 === '') {
							return '';
						} else {
							var styleName = _n1;
							return styleName + ('-' + name);
						}
					},
					subStyle));
		default:
			var x = style.a;
			return A2(
				elm$core$Maybe$withDefault,
				'',
				mdgriffith$elm_ui$Internal$Model$transformClass(x));
	}
};
var mdgriffith$elm_ui$Internal$Model$reduceStyles = F2(
	function (style, nevermind) {
		var cache = nevermind.a;
		var existing = nevermind.b;
		var styleName = mdgriffith$elm_ui$Internal$Model$getStyleName(style);
		return A2(elm$core$Set$member, styleName, cache) ? nevermind : _Utils_Tuple2(
			A2(elm$core$Set$insert, styleName, cache),
			A2(elm$core$List$cons, style, existing));
	});
var elm$core$Tuple$mapFirst = F2(
	function (func, _n0) {
		var x = _n0.a;
		var y = _n0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var elm$core$Tuple$mapSecond = F2(
	function (func, _n0) {
		var x = _n0.a;
		var y = _n0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var mdgriffith$elm_ui$Internal$Model$Property = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$Style = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$core$String$fromFloat = _String_fromNumber;
var mdgriffith$elm_ui$Internal$Model$formatColor = function (_n0) {
	var red = _n0.a;
	var green = _n0.b;
	var blue = _n0.c;
	var alpha = _n0.d;
	return 'rgba(' + (elm$core$String$fromInt(
		elm$core$Basics$round(red * 255)) + ((',' + elm$core$String$fromInt(
		elm$core$Basics$round(green * 255))) + ((',' + elm$core$String$fromInt(
		elm$core$Basics$round(blue * 255))) + (',' + (elm$core$String$fromFloat(alpha) + ')')))));
};
var mdgriffith$elm_ui$Internal$Model$formatBoxShadow = function (shadow) {
	return A2(
		elm$core$String$join,
		' ',
		A2(
			elm$core$List$filterMap,
			elm$core$Basics$identity,
			_List_fromArray(
				[
					shadow.cc ? elm$core$Maybe$Just('inset') : elm$core$Maybe$Nothing,
					elm$core$Maybe$Just(
					elm$core$String$fromFloat(shadow.es.a) + 'px'),
					elm$core$Maybe$Just(
					elm$core$String$fromFloat(shadow.es.b) + 'px'),
					elm$core$Maybe$Just(
					elm$core$String$fromFloat(shadow.di) + 'px'),
					elm$core$Maybe$Just(
					elm$core$String$fromFloat(shadow.e1) + 'px'),
					elm$core$Maybe$Just(
					mdgriffith$elm_ui$Internal$Model$formatColor(shadow.dB))
				])));
};
var mdgriffith$elm_ui$Internal$Style$dot = function (c) {
	return '.' + c;
};
var mdgriffith$elm_ui$Internal$Model$renderFocusStyle = function (focus) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$Style,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.da) + (':focus .focusable, ' + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.da) + '.focusable:focus')),
		A2(
			elm$core$List$filterMap,
			elm$core$Basics$identity,
			_List_fromArray(
				[
					A2(
					elm$core$Maybe$map,
					function (color) {
						return A2(
							mdgriffith$elm_ui$Internal$Model$Property,
							'border-color',
							mdgriffith$elm_ui$Internal$Model$formatColor(color));
					},
					focus.dm),
					A2(
					elm$core$Maybe$map,
					function (color) {
						return A2(
							mdgriffith$elm_ui$Internal$Model$Property,
							'background-color',
							mdgriffith$elm_ui$Internal$Model$formatColor(color));
					},
					focus.dd),
					A2(
					elm$core$Maybe$map,
					function (shadow) {
						return A2(
							mdgriffith$elm_ui$Internal$Model$Property,
							'box-shadow',
							mdgriffith$elm_ui$Internal$Model$formatBoxShadow(
								{
									di: shadow.di,
									dB: shadow.dB,
									cc: false,
									es: A2(
										elm$core$Tuple$mapSecond,
										elm$core$Basics$toFloat,
										A2(elm$core$Tuple$mapFirst, elm$core$Basics$toFloat, shadow.es)),
									e1: shadow.e1
								}));
					},
					focus.e$),
					elm$core$Maybe$Just(
					A2(mdgriffith$elm_ui$Internal$Model$Property, 'outline', 'none'))
				])));
};
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var mdgriffith$elm_ui$Internal$Style$Batch = function (a) {
	return {$: 5, a: a};
};
var mdgriffith$elm_ui$Internal$Style$Child = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Style$Class = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Style$Descriptor = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Style$Left = 3;
var mdgriffith$elm_ui$Internal$Style$Prop = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Style$Right = 2;
var mdgriffith$elm_ui$Internal$Style$Self = elm$core$Basics$identity;
var mdgriffith$elm_ui$Internal$Style$Supports = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var elm$core$List$concatMap = F2(
	function (f, list) {
		return elm$core$List$concat(
			A2(elm$core$List$map, f, list));
	});
var mdgriffith$elm_ui$Internal$Style$Content = elm$core$Basics$identity;
var mdgriffith$elm_ui$Internal$Style$Bottom = 1;
var mdgriffith$elm_ui$Internal$Style$CenterX = 4;
var mdgriffith$elm_ui$Internal$Style$CenterY = 5;
var mdgriffith$elm_ui$Internal$Style$Top = 0;
var mdgriffith$elm_ui$Internal$Style$alignments = _List_fromArray(
	[0, 1, 2, 3, 4, 5]);
var mdgriffith$elm_ui$Internal$Style$contentName = function (desc) {
	switch (desc) {
		case 0:
			var _n1 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dD);
		case 1:
			var _n2 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.a_);
		case 2:
			var _n3 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.a0);
		case 3:
			var _n4 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.aG);
		case 4:
			var _n5 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.a$);
		default:
			var _n6 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.T);
	}
};
var mdgriffith$elm_ui$Internal$Style$selfName = function (desc) {
	switch (desc) {
		case 0:
			var _n1 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.c8);
		case 1:
			var _n2 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.c1);
		case 2:
			var _n3 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bN);
		case 3:
			var _n4 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bM);
		case 4:
			var _n5 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.c2);
		default:
			var _n6 = desc;
			return mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.c3);
	}
};
var mdgriffith$elm_ui$Internal$Style$describeAlignment = function (values) {
	var createDescription = function (alignment) {
		var _n0 = values(alignment);
		var content = _n0.a;
		var indiv = _n0.b;
		return _List_fromArray(
			[
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$contentName(alignment),
				content),
				A2(
				mdgriffith$elm_ui$Internal$Style$Child,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.da),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$selfName(alignment),
						indiv)
					]))
			]);
	};
	return mdgriffith$elm_ui$Internal$Style$Batch(
		A2(elm$core$List$concatMap, createDescription, mdgriffith$elm_ui$Internal$Style$alignments));
};
var mdgriffith$elm_ui$Internal$Style$elDescription = _List_fromArray(
	[
		A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
		A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
		A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
		A2(
		mdgriffith$elm_ui$Internal$Style$Descriptor,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bt),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				mdgriffith$elm_ui$Internal$Style$Child,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.df),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
					]))
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Descriptor,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eU),
		_List_fromArray(
			[
				A2(
				mdgriffith$elm_ui$Internal$Style$Child,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cR),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bu),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bJ),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'auto !important')
							]))
					]))
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Child,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.a4),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Child,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bu),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Child,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bJ),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Child,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bI),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
			])),
		mdgriffith$elm_ui$Internal$Style$describeAlignment(
		function (alignment) {
			switch (alignment) {
				case 0:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
							]));
				case 1:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
							]));
				case 2:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
							]));
				case 3:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							]));
				case 4:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
							]));
				default:
					return _Utils_Tuple2(
						_List_fromArray(
							[
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.da),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
									]))
							]),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
							]));
			}
		})
	]);
var mdgriffith$elm_ui$Internal$Style$gridAlignments = function (values) {
	var createDescription = function (alignment) {
		return _List_fromArray(
			[
				A2(
				mdgriffith$elm_ui$Internal$Style$Child,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.da),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$selfName(alignment),
						values(alignment))
					]))
			]);
	};
	return mdgriffith$elm_ui$Internal$Style$Batch(
		A2(elm$core$List$concatMap, createDescription, mdgriffith$elm_ui$Internal$Style$alignments));
};
var mdgriffith$elm_ui$Internal$Style$Above = 0;
var mdgriffith$elm_ui$Internal$Style$Behind = 5;
var mdgriffith$elm_ui$Internal$Style$Below = 1;
var mdgriffith$elm_ui$Internal$Style$OnLeft = 3;
var mdgriffith$elm_ui$Internal$Style$OnRight = 2;
var mdgriffith$elm_ui$Internal$Style$Within = 4;
var mdgriffith$elm_ui$Internal$Style$locations = function () {
	var loc = 0;
	var _n0 = function () {
		switch (loc) {
			case 0:
				return 0;
			case 1:
				return 0;
			case 2:
				return 0;
			case 3:
				return 0;
			case 4:
				return 0;
			default:
				return 0;
		}
	}();
	return _List_fromArray(
		[0, 1, 2, 3, 4, 5]);
}();
var mdgriffith$elm_ui$Internal$Style$baseSheet = _List_fromArray(
	[
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		'html,body',
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		_Utils_ap(
			mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.da),
			_Utils_ap(
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e0),
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.d6))),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.da) + ':focus',
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'outline', 'none')
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eN),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'min-height', '100%'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.da),
					mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bu)),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bu),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%')
							]))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Child,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.d8),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.au),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed')
							]))
					]))
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.au),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e0),
				mdgriffith$elm_ui$Internal$Style$elDescription),
				mdgriffith$elm_ui$Internal$Style$Batch(
				function (fn) {
					return A2(elm$core$List$map, fn, mdgriffith$elm_ui$Internal$Style$locations);
				}(
					function (loc) {
						switch (loc) {
							case 0:
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.c_),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bu),
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												])),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bJ),
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
												])),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 1:
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dg),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'bottom', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												])),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bu),
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', 'auto')
												]))
										]));
							case 2:
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.ev),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 3:
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.et),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'right', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '20'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							case 4:
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.d8),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
							default:
								return A2(
									mdgriffith$elm_ui$Internal$Style$Descriptor,
									mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.df),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'absolute'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none'),
											A2(
											mdgriffith$elm_ui$Internal$Style$Child,
											'*',
											_List_fromArray(
												[
													A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto')
												]))
										]));
						}
					}))
			])),
		A2(
		mdgriffith$elm_ui$Internal$Style$Class,
		mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.da),
		_List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'relative'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border', 'none'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '0'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'resize', 'none'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', 'inherit'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'box-sizing', 'border-box'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'padding', '0'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-size', 'inherit'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'color', 'inherit'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-family', 'inherit'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', 'inherit'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'none'),
				A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'inherit'),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bK),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-wrap', 'wrap')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.ck),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, '-moz-user-select', 'none'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, '-webkit-user-select', 'none'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, '-ms-user-select', 'none'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'user-select', 'none')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dG),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'pointer')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dH),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'cursor', 'text')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eE),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none !important')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.aX),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'auto !important')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.am),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.aj),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.d3, mdgriffith$elm_ui$Internal$Style$classes.am)) + ':hover',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.d3, mdgriffith$elm_ui$Internal$Style$classes.aj)) + ':hover',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.dY, mdgriffith$elm_ui$Internal$Style$classes.am)) + ':focus',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.dY, mdgriffith$elm_ui$Internal$Style$classes.aj)) + ':focus',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.bm, mdgriffith$elm_ui$Internal$Style$classes.am)) + ':active',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '0')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(
					_Utils_ap(mdgriffith$elm_ui$Internal$Style$classes.bm, mdgriffith$elm_ui$Internal$Style$classes.aj)) + ':active',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'opacity', '1')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cT),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Internal$Style$Prop,
						'transition',
						A2(
							elm$core$String$join,
							', ',
							A2(
								elm$core$List$map,
								function (x) {
									return x + ' 160ms';
								},
								_List_fromArray(
									['transform', 'opacity', 'filter', 'background-color', 'color', 'font-size']))))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eR),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'auto'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eS),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'auto'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cH),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eT),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'auto'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.S),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e0),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-shrink', '1')
							]))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dy),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow', 'hidden')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dz),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-x', 'hidden')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dA),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'overflow-y', 'hidden')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bI),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', 'auto')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.aV),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-width', '0')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dn),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dashed')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.$7),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'dotted')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dp),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'border-style', 'solid')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cR),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-block')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.eb),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'line-height', '1.05')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e0),
				mdgriffith$elm_ui$Internal$Style$elDescription),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cH),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'row'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.da),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', '0%'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cX),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bu),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.b6),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bJ),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.aZ),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.c7,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.c5,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.c2),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-left', 'auto !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.c5,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.c2),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-right', 'auto !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.c5,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.c3),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + (mdgriffith$elm_ui$Internal$Style$classes.c5 + ' ~ u'),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + (mdgriffith$elm_ui$Internal$Style$classes.c7 + (' ~ s.' + mdgriffith$elm_ui$Internal$Style$classes.c5)),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 1:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 2:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_Nil);
								case 3:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_Nil);
								case 4:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
							}
						}),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e6),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							]))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.S),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-direction', 'column'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bu),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '100000')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bJ),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cY),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bI),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.c4,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:first-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.c6,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.c3),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', '0 !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.c6,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.c3),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', '0 !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:only-of-type.' + mdgriffith$elm_ui$Internal$Style$classes.c6,
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '1'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.c3),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto !important'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto !important')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						's:last-of-type.' + (mdgriffith$elm_ui$Internal$Style$classes.c6 + ' ~ u'),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						'u:first-of-type.' + (mdgriffith$elm_ui$Internal$Style$classes.c4 + (' ~ s.' + mdgriffith$elm_ui$Internal$Style$classes.c6)),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0')
							])),
						mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-bottom', 'auto')
											]));
								case 1:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin-top', 'auto')
											]));
								case 2:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-end')
											]));
								case 3:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'flex-start')
											]));
								case 4:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
											]),
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'center')
											]));
								default:
									return _Utils_Tuple2(
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
											]),
										_List_Nil);
							}
						}),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.aZ),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-grow', '0'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'flex-basis', 'auto'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-self', 'stretch !important')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e6),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'space-between')
							]))
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.d0),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', '-ms-grid'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						'.gp',
						_List_fromArray(
							[
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.da),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Supports,
						_Utils_Tuple2('display', 'grid'),
						_List_fromArray(
							[
								_Utils_Tuple2('display', 'grid')
							])),
						mdgriffith$elm_ui$Internal$Style$gridAlignments(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-start')
										]);
								case 1:
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'flex-end')
										]);
								case 2:
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-end')
										]);
								case 3:
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'flex-start')
										]);
								case 4:
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'align-items', 'center')
										]);
								default:
									return _List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Style$Prop, 'justify-content', 'center')
										]);
							}
						})
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cp),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.da + ':first-child'),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(
							mdgriffith$elm_ui$Internal$Style$classes.da + (mdgriffith$elm_ui$Internal$Style$selfName(3) + (':first-child + .' + mdgriffith$elm_ui$Internal$Style$classes.da))),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(
							mdgriffith$elm_ui$Internal$Style$classes.da + (mdgriffith$elm_ui$Internal$Style$selfName(2) + (':first-child + .' + mdgriffith$elm_ui$Internal$Style$classes.da))),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'margin', '0 !important')
							])),
						mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 1:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 2:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right'),
												A2(
												mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 3:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left'),
												A2(
												mdgriffith$elm_ui$Internal$Style$Descriptor,
												'::after',
												_List_fromArray(
													[
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'content', '\"\"'),
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'table'),
														A2(mdgriffith$elm_ui$Internal$Style$Prop, 'clear', 'both')
													]))
											]));
								case 4:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.ea),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'pre-wrap')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cr),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'block'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
						A2(
						mdgriffith$elm_ui$Internal$Style$Descriptor,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bt),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '0'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.df),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'z-index', '-1')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cR),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.e0),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal'),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.d8),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.df),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.c_),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dg),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.ev),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Descriptor,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.et),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'flex')
									])),
								A2(
								mdgriffith$elm_ui$Internal$Style$Child,
								mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cR),
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline'),
										A2(mdgriffith$elm_ui$Internal$Style$Prop, 'white-space', 'normal')
									]))
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cH),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-flex')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.S),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-flex')
							])),
						A2(
						mdgriffith$elm_ui$Internal$Style$Child,
						mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.d0),
						_List_fromArray(
							[
								A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'inline-grid')
							])),
						mdgriffith$elm_ui$Internal$Style$describeAlignment(
						function (alignment) {
							switch (alignment) {
								case 0:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 1:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								case 2:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'right')
											]));
								case 3:
									return _Utils_Tuple2(
										_List_Nil,
										_List_fromArray(
											[
												A2(mdgriffith$elm_ui$Internal$Style$Prop, 'float', 'left')
											]));
								case 4:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
								default:
									return _Utils_Tuple2(_List_Nil, _List_Nil);
							}
						})
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.hidden',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'display', 'none')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.ft),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '100')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fk),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '200')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fo),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '300')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fq),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '400')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fp),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '500')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fs),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '600')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.dl),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '700')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fj),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '800')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fl),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-weight', '900')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.ee),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'italic')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fd),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fH),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'underline'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				_Utils_ap(
					mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fH),
					mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fd)),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration', 'line-through underline'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip-ink', 'auto'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-decoration-skip', 'ink')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fu),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-style', 'normal')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fm),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.bi),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'justify-all')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fi),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'center')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fr),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'right')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.fn),
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'text-align', 'left')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Descriptor,
				'.modal',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'position', 'fixed'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'left', '0'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'top', '0'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'width', '100%'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'height', '100%'),
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'pointer-events', 'none')
					]))
			]))
	]);
var mdgriffith$elm_ui$Internal$Style$fontVariant = function (_var) {
	return _List_fromArray(
		[
			A2(
			mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + _var,
			_List_fromArray(
				[
					A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\"'))
				])),
			A2(
			mdgriffith$elm_ui$Internal$Style$Class,
			'.v-' + (_var + '-off'),
			_List_fromArray(
				[
					A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-feature-settings', '\"' + (_var + '\" 0'))
				]))
		]);
};
var mdgriffith$elm_ui$Internal$Style$commonValues = elm$core$List$concat(
	_List_fromArray(
		[
			A2(
			elm$core$List$map,
			function (x) {
				return A2(
					mdgriffith$elm_ui$Internal$Style$Class,
					'.border-' + elm$core$String$fromInt(x),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Internal$Style$Prop,
							'border-width',
							elm$core$String$fromInt(x) + 'px')
						]));
			},
			A2(elm$core$List$range, 0, 6)),
			A2(
			elm$core$List$map,
			function (i) {
				return A2(
					mdgriffith$elm_ui$Internal$Style$Class,
					'.font-size-' + elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Internal$Style$Prop,
							'font-size',
							elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2(elm$core$List$range, 8, 32)),
			A2(
			elm$core$List$map,
			function (i) {
				return A2(
					mdgriffith$elm_ui$Internal$Style$Class,
					'.p-' + elm$core$String$fromInt(i),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Internal$Style$Prop,
							'padding',
							elm$core$String$fromInt(i) + 'px')
						]));
			},
			A2(elm$core$List$range, 0, 24)),
			_List_fromArray(
			[
				A2(
				mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'small-caps')
					])),
				A2(
				mdgriffith$elm_ui$Internal$Style$Class,
				'.v-smcp-off',
				_List_fromArray(
					[
						A2(mdgriffith$elm_ui$Internal$Style$Prop, 'font-variant', 'normal')
					]))
			]),
			mdgriffith$elm_ui$Internal$Style$fontVariant('zero'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('onum'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('liga'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('dlig'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('ordn'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('tnum'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('afrc'),
			mdgriffith$elm_ui$Internal$Style$fontVariant('frac')
		]));
var mdgriffith$elm_ui$Internal$Style$explainer = '\n.explain {\n    border: 6px solid rgb(174, 121, 15) !important;\n}\n.explain > .' + (mdgriffith$elm_ui$Internal$Style$classes.da + (' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n.ctr {\n    border: none !important;\n}\n.explain > .ctr > .' + (mdgriffith$elm_ui$Internal$Style$classes.da + ' {\n    border: 4px dashed rgb(0, 151, 167) !important;\n}\n\n')));
var mdgriffith$elm_ui$Internal$Style$sliderOverrides = '\n\n/* General Input Reset */\ninput[type=range] {\n  -webkit-appearance: none; /* Hides the slider so that custom slider can be made */\n  /* width: 100%;  Specific width is required for Firefox. */\n  background: transparent; /* Otherwise white in Chrome */\n  position:absolute;\n  left:0;\n  top:0;\n  z-index:10;\n  width: 100%;\n  outline: dashed 1px;\n  height: 100%;\n  opacity: 0;\n}\n\n/* Hide all syling for track */\ninput[type=range]::-moz-range-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-ms-track {\n    background: transparent;\n    cursor: pointer;\n}\ninput[type=range]::-webkit-slider-runnable-track {\n    background: transparent;\n    cursor: pointer;\n}\n\n/* Thumbs */\ninput[type=range]::-webkit-slider-thumb {\n    -webkit-appearance: none;\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-moz-range-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range]::-ms-thumb {\n    opacity: 0.5;\n    width: 80px;\n    height: 80px;\n    background-color: black;\n    border:none;\n    border-radius: 5px;\n}\ninput[type=range][orient=vertical]{\n    writing-mode: bt-lr; /* IE */\n    -webkit-appearance: slider-vertical;  /* WebKit */\n}\n';
var mdgriffith$elm_ui$Internal$Style$overrides = '@media screen and (-ms-high-contrast: active), (-ms-high-contrast: none) {' + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.da) + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cH) + (' > ' + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.da) + (' { flex-basis: auto !important; } ' + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.da) + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.cH) + (' > ' + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.da) + (mdgriffith$elm_ui$Internal$Style$dot(mdgriffith$elm_ui$Internal$Style$classes.aZ) + (' { flex-basis: auto !important; }}' + (mdgriffith$elm_ui$Internal$Style$sliderOverrides + mdgriffith$elm_ui$Internal$Style$explainer))))))))))));
var elm$core$String$concat = function (strings) {
	return A2(elm$core$String$join, '', strings);
};
var mdgriffith$elm_ui$Internal$Style$Intermediate = elm$core$Basics$identity;
var mdgriffith$elm_ui$Internal$Style$emptyIntermediate = F2(
	function (selector, closing) {
		return {aY: closing, u: _List_Nil, X: _List_Nil, N: selector};
	});
var mdgriffith$elm_ui$Internal$Style$renderRules = F2(
	function (_n0, rulesToRender) {
		var parent = _n0;
		var generateIntermediates = F2(
			function (rule, rendered) {
				switch (rule.$) {
					case 0:
						var name = rule.a;
						var val = rule.b;
						return _Utils_update(
							rendered,
							{
								X: A2(
									elm$core$List$cons,
									_Utils_Tuple2(name, val),
									rendered.X)
							});
					case 2:
						var _n2 = rule.a;
						var prop = _n2.a;
						var value = _n2.b;
						var props = rule.b;
						return _Utils_update(
							rendered,
							{
								u: A2(
									elm$core$List$cons,
									{aY: '\n}', u: _List_Nil, X: props, N: '@supports (' + (prop + (':' + (value + (') {' + parent.N))))},
									rendered.u)
							});
					case 4:
						var selector = rule.a;
						var adjRules = rule.b;
						return _Utils_update(
							rendered,
							{
								u: A2(
									elm$core$List$cons,
									A2(
										mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.N + (' + ' + selector), ''),
										adjRules),
									rendered.u)
							});
					case 1:
						var child = rule.a;
						var childRules = rule.b;
						return _Utils_update(
							rendered,
							{
								u: A2(
									elm$core$List$cons,
									A2(
										mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.N + (' > ' + child), ''),
										childRules),
									rendered.u)
							});
					case 3:
						var descriptor = rule.a;
						var descriptorRules = rule.b;
						return _Utils_update(
							rendered,
							{
								u: A2(
									elm$core$List$cons,
									A2(
										mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(
											mdgriffith$elm_ui$Internal$Style$emptyIntermediate,
											_Utils_ap(parent.N, descriptor),
											''),
										descriptorRules),
									rendered.u)
							});
					default:
						var batched = rule.a;
						return _Utils_update(
							rendered,
							{
								u: A2(
									elm$core$List$cons,
									A2(
										mdgriffith$elm_ui$Internal$Style$renderRules,
										A2(mdgriffith$elm_ui$Internal$Style$emptyIntermediate, parent.N, ''),
										batched),
									rendered.u)
							});
				}
			});
		return A3(elm$core$List$foldr, generateIntermediates, parent, rulesToRender);
	});
var mdgriffith$elm_ui$Internal$Style$renderCompact = function (styleClasses) {
	var renderValues = function (values) {
		return elm$core$String$concat(
			A2(
				elm$core$List$map,
				function (_n3) {
					var x = _n3.a;
					var y = _n3.b;
					return x + (':' + (y + ';'));
				},
				values));
	};
	var renderClass = function (rule) {
		var _n2 = rule.X;
		if (!_n2.b) {
			return '';
		} else {
			return rule.N + ('{' + (renderValues(rule.X) + (rule.aY + '}')));
		}
	};
	var renderIntermediate = function (_n0) {
		var rule = _n0;
		return _Utils_ap(
			renderClass(rule),
			elm$core$String$concat(
				A2(elm$core$List$map, renderIntermediate, rule.u)));
	};
	return elm$core$String$concat(
		A2(
			elm$core$List$map,
			renderIntermediate,
			A3(
				elm$core$List$foldr,
				F2(
					function (_n1, existing) {
						var name = _n1.a;
						var styleRules = _n1.b;
						return A2(
							elm$core$List$cons,
							A2(
								mdgriffith$elm_ui$Internal$Style$renderRules,
								A2(mdgriffith$elm_ui$Internal$Style$emptyIntermediate, name, ''),
								styleRules),
							existing);
					}),
				_List_Nil,
				styleClasses)));
};
var mdgriffith$elm_ui$Internal$Style$rules = _Utils_ap(
	mdgriffith$elm_ui$Internal$Style$overrides,
	mdgriffith$elm_ui$Internal$Style$renderCompact(
		_Utils_ap(mdgriffith$elm_ui$Internal$Style$baseSheet, mdgriffith$elm_ui$Internal$Style$commonValues)));
var mdgriffith$elm_ui$Internal$Model$staticRoot = A3(
	elm$virtual_dom$VirtualDom$node,
	'style',
	_List_Nil,
	_List_fromArray(
		[
			elm$virtual_dom$VirtualDom$text(mdgriffith$elm_ui$Internal$Style$rules)
		]));
var elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var elm$core$Basics$negate = function (n) {
	return -n;
};
var elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var mdgriffith$elm_ui$Internal$Model$fontName = function (font) {
	switch (font.$) {
		case 0:
			return 'serif';
		case 1:
			return 'sans-serif';
		case 2:
			return 'monospace';
		case 3:
			var name = font.a;
			return '\"' + (name + '\"');
		case 4:
			var name = font.a;
			var url = font.b;
			return '\"' + (name + '\"');
		default:
			var name = font.a.eq;
			return '\"' + (name + '\"');
	}
};
var mdgriffith$elm_ui$Internal$Model$isSmallCaps = function (_var) {
	switch (_var.$) {
		case 0:
			var name = _var.a;
			return name === 'smcp';
		case 1:
			var name = _var.a;
			return false;
		default:
			var name = _var.a;
			var index = _var.b;
			return (name === 'smcp') && (index === 1);
	}
};
var mdgriffith$elm_ui$Internal$Model$hasSmallCaps = function (typeface) {
	if (typeface.$ === 5) {
		var font = typeface.a;
		return A2(elm$core$List$any, mdgriffith$elm_ui$Internal$Model$isSmallCaps, font.cU);
	} else {
		return false;
	}
};
var mdgriffith$elm_ui$Internal$Model$renderProps = F3(
	function (force, _n0, existing) {
		var key = _n0.a;
		var val = _n0.b;
		return force ? (existing + ('\n  ' + (key + (': ' + (val + ' !important;'))))) : (existing + ('\n  ' + (key + (': ' + (val + ';')))));
	});
var mdgriffith$elm_ui$Internal$Model$bracket = F2(
	function (selector, rules) {
		var renderPair = function (_n0) {
			var name = _n0.a;
			var val = _n0.b;
			return name + (': ' + (val + ';'));
		};
		return selector + (' {' + (A2(
			elm$core$String$join,
			'',
			A2(elm$core$List$map, renderPair, rules)) + '}'));
	});
var mdgriffith$elm_ui$Internal$Model$fontRule = F3(
	function (name, modifier, _n0) {
		var parentAdj = _n0.a;
		var textAdjustment = _n0.b;
		return _List_fromArray(
			[
				A2(mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + (', ' + ('.' + (name + (' .' + modifier))))))), parentAdj),
				A2(mdgriffith$elm_ui$Internal$Model$bracket, '.' + (name + ('.' + (modifier + ('> .' + (mdgriffith$elm_ui$Internal$Style$classes.cR + (', .' + (name + (' .' + (modifier + (' > .' + mdgriffith$elm_ui$Internal$Style$classes.cR)))))))))), textAdjustment)
			]);
	});
var mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule = F3(
	function (fontToAdjust, _n0, otherFontName) {
		var full = _n0.a;
		var capital = _n0.b;
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			elm$core$String$join,
			' ',
			_Utils_ap(
				A3(mdgriffith$elm_ui$Internal$Model$fontRule, name, mdgriffith$elm_ui$Internal$Style$classes.e2, capital),
				A3(mdgriffith$elm_ui$Internal$Model$fontRule, name, mdgriffith$elm_ui$Internal$Style$classes.d$, full)));
	});
var mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule = F2(
	function (fontToAdjust, otherFontName) {
		var name = _Utils_eq(fontToAdjust, otherFontName) ? fontToAdjust : (otherFontName + (' .' + fontToAdjust));
		return A2(
			elm$core$String$join,
			' ',
			_List_fromArray(
				[
					A2(
					mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.e2 + (', ' + ('.' + (name + (' .' + mdgriffith$elm_ui$Internal$Style$classes.e2))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('line-height', '1')
						])),
					A2(
					mdgriffith$elm_ui$Internal$Model$bracket,
					'.' + (name + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.e2 + ('> .' + (mdgriffith$elm_ui$Internal$Style$classes.cR + (', .' + (name + (' .' + (mdgriffith$elm_ui$Internal$Style$classes.e2 + (' > .' + mdgriffith$elm_ui$Internal$Style$classes.cR)))))))))),
					_List_fromArray(
						[
							_Utils_Tuple2('vertical-align', '0'),
							_Utils_Tuple2('line-height', '1')
						]))
				]));
	});
var elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(
			A3(elm$core$List$foldl, elm$core$Basics$max, x, xs));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(
			A3(elm$core$List$foldl, elm$core$Basics$min, x, xs));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var mdgriffith$elm_ui$Internal$Model$adjust = F3(
	function (size, height, vertical) {
		return {as: height / size, e1: size, cV: vertical};
	});
var mdgriffith$elm_ui$Internal$Model$convertAdjustment = function (adjustment) {
	var lines = _List_fromArray(
		[adjustment.dt, adjustment.de, adjustment.dK, adjustment.ei]);
	var lineHeight = 1.5;
	var normalDescender = (lineHeight - 1) / 2;
	var oldMiddle = lineHeight / 2;
	var descender = A2(
		elm$core$Maybe$withDefault,
		adjustment.dK,
		elm$core$List$minimum(lines));
	var newBaseline = A2(
		elm$core$Maybe$withDefault,
		adjustment.de,
		elm$core$List$minimum(
			A2(
				elm$core$List$filter,
				function (x) {
					return !_Utils_eq(x, descender);
				},
				lines)));
	var base = lineHeight;
	var ascender = A2(
		elm$core$Maybe$withDefault,
		adjustment.dt,
		elm$core$List$maximum(lines));
	var capitalSize = 1 / (ascender - newBaseline);
	var capitalVertical = 1 - ascender;
	var fullSize = 1 / (ascender - descender);
	var fullVertical = 1 - ascender;
	var newCapitalMiddle = ((ascender - newBaseline) / 2) + newBaseline;
	var newFullMiddle = ((ascender - descender) / 2) + descender;
	return {
		dt: A3(mdgriffith$elm_ui$Internal$Model$adjust, capitalSize, ascender - newBaseline, capitalVertical),
		b1: A3(mdgriffith$elm_ui$Internal$Model$adjust, fullSize, ascender - descender, fullVertical)
	};
};
var mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules = function (converted) {
	return _Utils_Tuple2(
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'block')
			]),
		_List_fromArray(
			[
				_Utils_Tuple2('display', 'inline-block'),
				_Utils_Tuple2(
				'line-height',
				elm$core$String$fromFloat(converted.as)),
				_Utils_Tuple2(
				'vertical-align',
				elm$core$String$fromFloat(converted.cV) + 'em'),
				_Utils_Tuple2(
				'font-size',
				elm$core$String$fromFloat(converted.e1) + 'em')
			]));
};
var mdgriffith$elm_ui$Internal$Model$typefaceAdjustment = function (typefaces) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (face, found) {
				if (found.$ === 1) {
					if (face.$ === 5) {
						var _with = face.a;
						var _n2 = _with.c0;
						if (_n2.$ === 1) {
							return found;
						} else {
							var adjustment = _n2.a;
							return elm$core$Maybe$Just(
								_Utils_Tuple2(
									mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.b1;
										}(
											mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment))),
									mdgriffith$elm_ui$Internal$Model$fontAdjustmentRules(
										function ($) {
											return $.dt;
										}(
											mdgriffith$elm_ui$Internal$Model$convertAdjustment(adjustment)))));
						}
					} else {
						return found;
					}
				} else {
					return found;
				}
			}),
		elm$core$Maybe$Nothing,
		typefaces);
};
var mdgriffith$elm_ui$Internal$Model$renderTopLevelValues = function (rules) {
	var withImport = function (font) {
		if (font.$ === 4) {
			var url = font.b;
			return elm$core$Maybe$Just('@import url(\'' + (url + '\');'));
		} else {
			return elm$core$Maybe$Nothing;
		}
	};
	var fontImports = function (_n2) {
		var name = _n2.a;
		var typefaces = _n2.b;
		var imports = A2(
			elm$core$String$join,
			'\n',
			A2(elm$core$List$filterMap, withImport, typefaces));
		return imports;
	};
	var allNames = A2(elm$core$List$map, elm$core$Tuple$first, rules);
	var fontAdjustments = function (_n1) {
		var name = _n1.a;
		var typefaces = _n1.b;
		var _n0 = mdgriffith$elm_ui$Internal$Model$typefaceAdjustment(typefaces);
		if (_n0.$ === 1) {
			return A2(
				elm$core$String$join,
				'',
				A2(
					elm$core$List$map,
					mdgriffith$elm_ui$Internal$Model$renderNullAdjustmentRule(name),
					allNames));
		} else {
			var adjustment = _n0.a;
			return A2(
				elm$core$String$join,
				'',
				A2(
					elm$core$List$map,
					A2(mdgriffith$elm_ui$Internal$Model$renderFontAdjustmentRule, name, adjustment),
					allNames));
		}
	};
	return _Utils_ap(
		A2(
			elm$core$String$join,
			'\n',
			A2(elm$core$List$map, fontImports, rules)),
		A2(
			elm$core$String$join,
			'\n',
			A2(elm$core$List$map, fontAdjustments, rules)));
};
var mdgriffith$elm_ui$Internal$Model$renderVariant = function (_var) {
	switch (_var.$) {
		case 0:
			var name = _var.a;
			return '\"' + (name + '\"');
		case 1:
			var name = _var.a;
			return '\"' + (name + '\" 0');
		default:
			var name = _var.a;
			var index = _var.b;
			return '\"' + (name + ('\" ' + elm$core$String$fromInt(index)));
	}
};
var mdgriffith$elm_ui$Internal$Model$renderVariants = function (typeface) {
	if (typeface.$ === 5) {
		var font = typeface.a;
		return elm$core$Maybe$Just(
			A2(
				elm$core$String$join,
				', ',
				A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$renderVariant, font.cU)));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var mdgriffith$elm_ui$Internal$Model$topLevelValue = function (rule) {
	if (rule.$ === 1) {
		var name = rule.a;
		var typefaces = rule.b;
		return elm$core$Maybe$Just(
			_Utils_Tuple2(name, typefaces));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var mdgriffith$elm_ui$Internal$Model$transformValue = function (transform) {
	switch (transform.$) {
		case 0:
			return elm$core$Maybe$Nothing;
		case 1:
			var _n1 = transform.a;
			var x = _n1.a;
			var y = _n1.b;
			var z = _n1.c;
			return elm$core$Maybe$Just(
				'translate3d(' + (elm$core$String$fromFloat(x) + ('px, ' + (elm$core$String$fromFloat(y) + ('px, ' + (elm$core$String$fromFloat(z) + 'px)'))))));
		default:
			var _n2 = transform.a;
			var tx = _n2.a;
			var ty = _n2.b;
			var tz = _n2.c;
			var _n3 = transform.b;
			var sx = _n3.a;
			var sy = _n3.b;
			var sz = _n3.c;
			var _n4 = transform.c;
			var ox = _n4.a;
			var oy = _n4.b;
			var oz = _n4.c;
			var angle = transform.d;
			var translate = 'translate3d(' + (elm$core$String$fromFloat(tx) + ('px, ' + (elm$core$String$fromFloat(ty) + ('px, ' + (elm$core$String$fromFloat(tz) + 'px)')))));
			var scale = 'scale3d(' + (elm$core$String$fromFloat(sx) + (', ' + (elm$core$String$fromFloat(sy) + (', ' + (elm$core$String$fromFloat(sz) + ')')))));
			var rotate = 'rotate3d(' + (elm$core$String$fromFloat(ox) + (', ' + (elm$core$String$fromFloat(oy) + (', ' + (elm$core$String$fromFloat(oz) + (', ' + (elm$core$String$fromFloat(angle) + 'rad)')))))));
			return elm$core$Maybe$Just(translate + (' ' + (scale + (' ' + rotate))));
	}
};
var mdgriffith$elm_ui$Internal$Model$toStyleSheetString = F2(
	function (options, stylesheet) {
		var renderStyle = F3(
			function (maybePseudo, selector, props) {
				if (maybePseudo.$ === 1) {
					return selector + ('{' + (A3(
						elm$core$List$foldl,
						mdgriffith$elm_ui$Internal$Model$renderProps(false),
						'',
						props) + '\n}'));
				} else {
					var pseudo = maybePseudo.a;
					switch (pseudo) {
						case 1:
							var _n17 = options.d3;
							switch (_n17) {
								case 0:
									return '';
								case 2:
									return selector + ('-hv {' + (A3(
										elm$core$List$foldl,
										mdgriffith$elm_ui$Internal$Model$renderProps(true),
										'',
										props) + '\n}'));
								default:
									return selector + ('-hv:hover {' + (A3(
										elm$core$List$foldl,
										mdgriffith$elm_ui$Internal$Model$renderProps(false),
										'',
										props) + '\n}'));
							}
						case 0:
							var renderedProps = A3(
								elm$core$List$foldl,
								mdgriffith$elm_ui$Internal$Model$renderProps(false),
								'',
								props);
							return A2(
								elm$core$String$join,
								'\n',
								_List_fromArray(
									[selector + ('-fs:focus {' + (renderedProps + '\n}')), '.' + (mdgriffith$elm_ui$Internal$Style$classes.da + (':focus ~ ' + (selector + ('-fs:not(.focus)  {' + (renderedProps + '\n}'))))), '.' + (mdgriffith$elm_ui$Internal$Style$classes.da + (':focus ' + (selector + ('-fs  {' + (renderedProps + '\n}'))))), '.focusable-parent:focus ~ ' + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.da + (' ' + (selector + ('-fs {' + (renderedProps + '\n}'))))))]));
						default:
							return selector + ('-act:active {' + (A3(
								elm$core$List$foldl,
								mdgriffith$elm_ui$Internal$Model$renderProps(false),
								'',
								props) + '\n}'));
					}
				}
			});
		var renderStyleRule = F2(
			function (rule, maybePseudo) {
				switch (rule.$) {
					case 0:
						var selector = rule.a;
						var props = rule.b;
						return A3(renderStyle, maybePseudo, selector, props);
					case 13:
						var name = rule.a;
						var prop = rule.b;
						return A3(
							renderStyle,
							maybePseudo,
							'.' + name,
							_List_fromArray(
								[
									A2(mdgriffith$elm_ui$Internal$Model$Property, 'box-shadow', prop)
								]));
					case 12:
						var name = rule.a;
						var transparency = rule.b;
						var opacity = A2(
							elm$core$Basics$max,
							0,
							A2(elm$core$Basics$min, 1, 1 - transparency));
						return A3(
							renderStyle,
							maybePseudo,
							'.' + name,
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Internal$Model$Property,
									'opacity',
									elm$core$String$fromFloat(opacity))
								]));
					case 2:
						var i = rule.a;
						return A3(
							renderStyle,
							maybePseudo,
							'.font-size-' + elm$core$String$fromInt(i),
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Internal$Model$Property,
									'font-size',
									elm$core$String$fromInt(i) + 'px')
								]));
					case 1:
						var name = rule.a;
						var typefaces = rule.b;
						var features = A2(
							elm$core$String$join,
							', ',
							A2(elm$core$List$filterMap, mdgriffith$elm_ui$Internal$Model$renderVariants, typefaces));
						var families = _List_fromArray(
							[
								A2(
								mdgriffith$elm_ui$Internal$Model$Property,
								'font-family',
								A2(
									elm$core$String$join,
									', ',
									A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$fontName, typefaces))),
								A2(mdgriffith$elm_ui$Internal$Model$Property, 'font-feature-settings', features),
								A2(
								mdgriffith$elm_ui$Internal$Model$Property,
								'font-variant',
								A2(elm$core$List$any, mdgriffith$elm_ui$Internal$Model$hasSmallCaps, typefaces) ? 'small-caps' : 'normal')
							]);
						return A2(
							elm$core$String$join,
							' ',
							_List_fromArray(
								[
									A3(renderStyle, maybePseudo, '.' + name, families)
								]));
					case 3:
						var _class = rule.a;
						var prop = rule.b;
						var val = rule.c;
						return A3(
							renderStyle,
							maybePseudo,
							'.' + _class,
							_List_fromArray(
								[
									A2(mdgriffith$elm_ui$Internal$Model$Property, prop, val)
								]));
					case 4:
						var _class = rule.a;
						var prop = rule.b;
						var color = rule.c;
						return A3(
							renderStyle,
							maybePseudo,
							'.' + _class,
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Internal$Model$Property,
									prop,
									mdgriffith$elm_ui$Internal$Model$formatColor(color))
								]));
					case 5:
						var cls = rule.a;
						var x = rule.b;
						var y = rule.c;
						var yPx = elm$core$String$fromInt(y) + 'px';
						var xPx = elm$core$String$fromInt(x) + 'px';
						var single = '.' + mdgriffith$elm_ui$Internal$Style$classes.e0;
						var row = '.' + mdgriffith$elm_ui$Internal$Style$classes.cH;
						var wrappedRow = '.' + (mdgriffith$elm_ui$Internal$Style$classes.bK + row);
						var right = '.' + mdgriffith$elm_ui$Internal$Style$classes.bN;
						var paragraph = '.' + mdgriffith$elm_ui$Internal$Style$classes.cr;
						var page = '.' + mdgriffith$elm_ui$Internal$Style$classes.cp;
						var left = '.' + mdgriffith$elm_ui$Internal$Style$classes.bM;
						var halfY = elm$core$String$fromFloat(y / 2) + 'px';
						var halfX = elm$core$String$fromFloat(x / 2) + 'px';
						var column = '.' + mdgriffith$elm_ui$Internal$Style$classes.S;
						var _class = '.' + cls;
						var any = '.' + mdgriffith$elm_ui$Internal$Style$classes.da;
						return elm$core$String$concat(
							_List_fromArray(
								[
									A3(
									renderStyle,
									maybePseudo,
									_class + (row + (' > ' + (any + (' + ' + any)))),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (wrappedRow + (' > ' + any)),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin', halfY + (' ' + halfX))
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (column + (' > ' + (any + (' + ' + any)))),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (page + (' > ' + (any + (' + ' + any)))),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-top', yPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (page + (' > ' + left)),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (page + (' > ' + right)),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_Utils_ap(_class, paragraph),
									_List_fromArray(
										[
											A2(
											mdgriffith$elm_ui$Internal$Model$Property,
											'line-height',
											'calc(1em + ' + (elm$core$String$fromInt(y) + 'px)'))
										])),
									A3(
									renderStyle,
									maybePseudo,
									'textarea' + _class,
									_List_fromArray(
										[
											A2(
											mdgriffith$elm_ui$Internal$Model$Property,
											'line-height',
											'calc(1em + ' + (elm$core$String$fromInt(y) + 'px)'))
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (paragraph + (' > ' + left)),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-right', xPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (paragraph + (' > ' + right)),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'margin-left', xPx)
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (paragraph + '::after'),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
											A2(
											mdgriffith$elm_ui$Internal$Model$Property,
											'margin-top',
											elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
										])),
									A3(
									renderStyle,
									maybePseudo,
									_class + (paragraph + '::before'),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'content', '\'\''),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'display', 'block'),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'height', '0'),
											A2(mdgriffith$elm_ui$Internal$Model$Property, 'width', '0'),
											A2(
											mdgriffith$elm_ui$Internal$Model$Property,
											'margin-bottom',
											elm$core$String$fromInt((-1) * ((y / 2) | 0)) + 'px')
										]))
								]));
					case 7:
						var cls = rule.a;
						var top = rule.b;
						var right = rule.c;
						var bottom = rule.d;
						var left = rule.e;
						var _class = '.' + cls;
						return A3(
							renderStyle,
							maybePseudo,
							_class,
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Internal$Model$Property,
									'padding',
									elm$core$String$fromInt(top) + ('px ' + (elm$core$String$fromInt(right) + ('px ' + (elm$core$String$fromInt(bottom) + ('px ' + (elm$core$String$fromInt(left) + 'px')))))))
								]));
					case 6:
						var cls = rule.a;
						var top = rule.b;
						var right = rule.c;
						var bottom = rule.d;
						var left = rule.e;
						var _class = '.' + cls;
						return A3(
							renderStyle,
							maybePseudo,
							_class,
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Internal$Model$Property,
									'border-width',
									elm$core$String$fromInt(top) + ('px ' + (elm$core$String$fromInt(right) + ('px ' + (elm$core$String$fromInt(bottom) + ('px ' + (elm$core$String$fromInt(left) + 'px')))))))
								]));
					case 8:
						var template = rule.a;
						var toGridLengthHelper = F3(
							function (minimum, maximum, x) {
								toGridLengthHelper:
								while (true) {
									switch (x.$) {
										case 0:
											var px = x.a;
											return elm$core$String$fromInt(px) + 'px';
										case 1:
											var _n2 = _Utils_Tuple2(minimum, maximum);
											if (_n2.a.$ === 1) {
												if (_n2.b.$ === 1) {
													var _n3 = _n2.a;
													var _n4 = _n2.b;
													return 'max-content';
												} else {
													var _n6 = _n2.a;
													var maxSize = _n2.b.a;
													return 'minmax(max-content, ' + (elm$core$String$fromInt(maxSize) + 'px)');
												}
											} else {
												if (_n2.b.$ === 1) {
													var minSize = _n2.a.a;
													var _n5 = _n2.b;
													return 'minmax(' + (elm$core$String$fromInt(minSize) + ('px, ' + 'max-content)'));
												} else {
													var minSize = _n2.a.a;
													var maxSize = _n2.b.a;
													return 'minmax(' + (elm$core$String$fromInt(minSize) + ('px, ' + (elm$core$String$fromInt(maxSize) + 'px)')));
												}
											}
										case 2:
											var i = x.a;
											var _n7 = _Utils_Tuple2(minimum, maximum);
											if (_n7.a.$ === 1) {
												if (_n7.b.$ === 1) {
													var _n8 = _n7.a;
													var _n9 = _n7.b;
													return elm$core$String$fromInt(i) + 'fr';
												} else {
													var _n11 = _n7.a;
													var maxSize = _n7.b.a;
													return 'minmax(max-content, ' + (elm$core$String$fromInt(maxSize) + 'px)');
												}
											} else {
												if (_n7.b.$ === 1) {
													var minSize = _n7.a.a;
													var _n10 = _n7.b;
													return 'minmax(' + (elm$core$String$fromInt(minSize) + ('px, ' + (elm$core$String$fromInt(i) + ('fr' + 'fr)'))));
												} else {
													var minSize = _n7.a.a;
													var maxSize = _n7.b.a;
													return 'minmax(' + (elm$core$String$fromInt(minSize) + ('px, ' + (elm$core$String$fromInt(maxSize) + 'px)')));
												}
											}
										case 3:
											var m = x.a;
											var len = x.b;
											var $temp$minimum = elm$core$Maybe$Just(m),
												$temp$maximum = maximum,
												$temp$x = len;
											minimum = $temp$minimum;
											maximum = $temp$maximum;
											x = $temp$x;
											continue toGridLengthHelper;
										default:
											var m = x.a;
											var len = x.b;
											var $temp$minimum = minimum,
												$temp$maximum = elm$core$Maybe$Just(m),
												$temp$x = len;
											minimum = $temp$minimum;
											maximum = $temp$maximum;
											x = $temp$x;
											continue toGridLengthHelper;
									}
								}
							});
						var toGridLength = function (x) {
							return A3(toGridLengthHelper, elm$core$Maybe$Nothing, elm$core$Maybe$Nothing, x);
						};
						var xSpacing = toGridLength(template.e7.a);
						var ySpacing = toGridLength(template.e7.b);
						var rows = function (x) {
							return 'grid-template-rows: ' + (x + ';');
						}(
							A2(
								elm$core$String$join,
								' ',
								A2(elm$core$List$map, toGridLength, template.eO)));
						var msRows = function (x) {
							return '-ms-grid-rows: ' + (x + ';');
						}(
							A2(
								elm$core$String$join,
								ySpacing,
								A2(elm$core$List$map, toGridLength, template.J)));
						var msColumns = function (x) {
							return '-ms-grid-columns: ' + (x + ';');
						}(
							A2(
								elm$core$String$join,
								ySpacing,
								A2(elm$core$List$map, toGridLength, template.J)));
						var gapY = 'grid-row-gap:' + (toGridLength(template.e7.b) + ';');
						var gapX = 'grid-column-gap:' + (toGridLength(template.e7.a) + ';');
						var columns = function (x) {
							return 'grid-template-columns: ' + (x + ';');
						}(
							A2(
								elm$core$String$join,
								' ',
								A2(elm$core$List$map, toGridLength, template.J)));
						var _class = '.grid-rows-' + (A2(
							elm$core$String$join,
							'-',
							A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$lengthClassName, template.eO)) + ('-cols-' + (A2(
							elm$core$String$join,
							'-',
							A2(elm$core$List$map, mdgriffith$elm_ui$Internal$Model$lengthClassName, template.J)) + ('-space-x-' + (mdgriffith$elm_ui$Internal$Model$lengthClassName(template.e7.a) + ('-space-y-' + mdgriffith$elm_ui$Internal$Model$lengthClassName(template.e7.b)))))));
						var modernGrid = _class + ('{' + (columns + (rows + (gapX + (gapY + '}')))));
						var supports = '@supports (display:grid) {' + (modernGrid + '}');
						var base = _class + ('{' + (msColumns + (msRows + '}')));
						return _Utils_ap(base, supports);
					case 9:
						var position = rule.a;
						var msPosition = A2(
							elm$core$String$join,
							' ',
							_List_fromArray(
								[
									'-ms-grid-row: ' + (elm$core$String$fromInt(position.cH) + ';'),
									'-ms-grid-row-span: ' + (elm$core$String$fromInt(position.as) + ';'),
									'-ms-grid-column: ' + (elm$core$String$fromInt(position.bU) + ';'),
									'-ms-grid-column-span: ' + (elm$core$String$fromInt(position.aE) + ';')
								]));
						var modernPosition = A2(
							elm$core$String$join,
							' ',
							_List_fromArray(
								[
									'grid-row: ' + (elm$core$String$fromInt(position.cH) + (' / ' + (elm$core$String$fromInt(position.cH + position.as) + ';'))),
									'grid-column: ' + (elm$core$String$fromInt(position.bU) + (' / ' + (elm$core$String$fromInt(position.bU + position.aE) + ';')))
								]));
						var _class = '.grid-pos-' + (elm$core$String$fromInt(position.cH) + ('-' + (elm$core$String$fromInt(position.bU) + ('-' + (elm$core$String$fromInt(position.aE) + ('-' + elm$core$String$fromInt(position.as)))))));
						var modernGrid = _class + ('{' + (modernPosition + '}'));
						var supports = '@supports (display:grid) {' + (modernGrid + '}');
						var base = _class + ('{' + (msPosition + '}'));
						return _Utils_ap(base, supports);
					case 11:
						var _class = rule.a;
						var styles = rule.b;
						var renderPseudoRule = function (style) {
							return A2(
								renderStyleRule,
								style,
								elm$core$Maybe$Just(_class));
						};
						return A2(
							elm$core$String$join,
							' ',
							A2(elm$core$List$map, renderPseudoRule, styles));
					default:
						var transform = rule.a;
						var val = mdgriffith$elm_ui$Internal$Model$transformValue(transform);
						var _class = mdgriffith$elm_ui$Internal$Model$transformClass(transform);
						var _n12 = _Utils_Tuple2(_class, val);
						if ((!_n12.a.$) && (!_n12.b.$)) {
							var cls = _n12.a.a;
							var v = _n12.b.a;
							return A3(
								renderStyle,
								maybePseudo,
								'.' + cls,
								_List_fromArray(
									[
										A2(mdgriffith$elm_ui$Internal$Model$Property, 'transform', v)
									]));
						} else {
							return '';
						}
				}
			});
		var combine = F2(
			function (style, rendered) {
				return {
					bf: _Utils_ap(
						rendered.bf,
						A2(renderStyleRule, style, elm$core$Maybe$Nothing)),
					aS: function () {
						var _n14 = mdgriffith$elm_ui$Internal$Model$topLevelValue(style);
						if (_n14.$ === 1) {
							return rendered.aS;
						} else {
							var topLevel = _n14.a;
							return A2(elm$core$List$cons, topLevel, rendered.aS);
						}
					}()
				};
			});
		var _n13 = A3(
			elm$core$List$foldl,
			combine,
			{bf: '', aS: _List_Nil},
			stylesheet);
		var topLevel = _n13.aS;
		var rules = _n13.bf;
		return _Utils_ap(
			mdgriffith$elm_ui$Internal$Model$renderTopLevelValues(topLevel),
			rules);
	});
var mdgriffith$elm_ui$Internal$Model$toStyleSheet = F2(
	function (options, styleSheet) {
		return A3(
			elm$virtual_dom$VirtualDom$node,
			'style',
			_List_Nil,
			_List_fromArray(
				[
					elm$virtual_dom$VirtualDom$text(
					A2(mdgriffith$elm_ui$Internal$Model$toStyleSheetString, options, styleSheet))
				]));
	});
var mdgriffith$elm_ui$Internal$Model$embedKeyed = F4(
	function (_static, opts, styles, children) {
		return _static ? A2(
			elm$core$List$cons,
			_Utils_Tuple2('static-stylesheet', mdgriffith$elm_ui$Internal$Model$staticRoot),
			A2(
				elm$core$List$cons,
				_Utils_Tuple2(
					'dynamic-stylesheet',
					A2(
						mdgriffith$elm_ui$Internal$Model$toStyleSheet,
						opts,
						A3(
							elm$core$List$foldl,
							mdgriffith$elm_ui$Internal$Model$reduceStyles,
							_Utils_Tuple2(
								elm$core$Set$empty,
								_List_fromArray(
									[
										mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.dY)
									])),
							styles).b)),
				children)) : A2(
			elm$core$List$cons,
			_Utils_Tuple2(
				'dynamic-stylesheet',
				A2(
					mdgriffith$elm_ui$Internal$Model$toStyleSheet,
					opts,
					A3(
						elm$core$List$foldl,
						mdgriffith$elm_ui$Internal$Model$reduceStyles,
						_Utils_Tuple2(
							elm$core$Set$empty,
							_List_fromArray(
								[
									mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.dY)
								])),
						styles).b)),
			children);
	});
var mdgriffith$elm_ui$Internal$Model$embedWith = F4(
	function (_static, opts, styles, children) {
		return _static ? A2(
			elm$core$List$cons,
			mdgriffith$elm_ui$Internal$Model$staticRoot,
			A2(
				elm$core$List$cons,
				A2(
					mdgriffith$elm_ui$Internal$Model$toStyleSheet,
					opts,
					A3(
						elm$core$List$foldl,
						mdgriffith$elm_ui$Internal$Model$reduceStyles,
						_Utils_Tuple2(
							elm$core$Set$empty,
							_List_fromArray(
								[
									mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.dY)
								])),
						styles).b),
				children)) : A2(
			elm$core$List$cons,
			A2(
				mdgriffith$elm_ui$Internal$Model$toStyleSheet,
				opts,
				A3(
					elm$core$List$foldl,
					mdgriffith$elm_ui$Internal$Model$reduceStyles,
					_Utils_Tuple2(
						elm$core$Set$empty,
						_List_fromArray(
							[
								mdgriffith$elm_ui$Internal$Model$renderFocusStyle(opts.dY)
							])),
					styles).b),
			children);
	});
var mdgriffith$elm_ui$Internal$Model$finalizeNode = F6(
	function (has, node, attributes, children, embedMode, parentContext) {
		var createNode = F2(
			function (nodeName, attrs) {
				if (children.$ === 1) {
					var keyed = children.a;
					return A3(
						elm$virtual_dom$VirtualDom$keyedNode,
						nodeName,
						attrs,
						function () {
							switch (embedMode.$) {
								case 0:
									return keyed;
								case 2:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4(mdgriffith$elm_ui$Internal$Model$embedKeyed, false, opts, styles, keyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4(mdgriffith$elm_ui$Internal$Model$embedKeyed, true, opts, styles, keyed);
							}
						}());
				} else {
					var unkeyed = children.a;
					return A2(
						function () {
							switch (nodeName) {
								case 'div':
									return elm$html$Html$div;
								case 'p':
									return elm$html$Html$p;
								default:
									return elm$virtual_dom$VirtualDom$node(nodeName);
							}
						}(),
						attrs,
						function () {
							switch (embedMode.$) {
								case 0:
									return unkeyed;
								case 2:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4(mdgriffith$elm_ui$Internal$Model$embedWith, false, opts, styles, unkeyed);
								default:
									var opts = embedMode.a;
									var styles = embedMode.b;
									return A4(mdgriffith$elm_ui$Internal$Model$embedWith, true, opts, styles, unkeyed);
							}
						}());
				}
			});
		var html = function () {
			switch (node.$) {
				case 0:
					return A2(createNode, 'div', attributes);
				case 1:
					var nodeName = node.a;
					return A2(createNode, nodeName, attributes);
				default:
					var nodeName = node.a;
					var internal = node.b;
					return A3(
						elm$virtual_dom$VirtualDom$node,
						nodeName,
						attributes,
						_List_fromArray(
							[
								A2(
								createNode,
								internal,
								_List_fromArray(
									[
										elm$html$Html$Attributes$class(mdgriffith$elm_ui$Internal$Style$classes.da + (' ' + mdgriffith$elm_ui$Internal$Style$classes.e0))
									]))
							]));
			}
		}();
		switch (parentContext) {
			case 0:
				return (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$widthFill, has) && (!A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$widthBetween, has))) ? html : (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$alignRight, has) ? A2(
					elm$html$Html$u,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class(
							A2(
								elm$core$String$join,
								' ',
								_List_fromArray(
									[mdgriffith$elm_ui$Internal$Style$classes.da, mdgriffith$elm_ui$Internal$Style$classes.e0, mdgriffith$elm_ui$Internal$Style$classes.aZ, mdgriffith$elm_ui$Internal$Style$classes.T, mdgriffith$elm_ui$Internal$Style$classes.c7])))
						]),
					_List_fromArray(
						[html])) : (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$centerX, has) ? A2(
					elm$html$Html$s,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class(
							A2(
								elm$core$String$join,
								' ',
								_List_fromArray(
									[mdgriffith$elm_ui$Internal$Style$classes.da, mdgriffith$elm_ui$Internal$Style$classes.e0, mdgriffith$elm_ui$Internal$Style$classes.aZ, mdgriffith$elm_ui$Internal$Style$classes.T, mdgriffith$elm_ui$Internal$Style$classes.c5])))
						]),
					_List_fromArray(
						[html])) : html));
			case 1:
				return (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$heightFill, has) && (!A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$heightBetween, has))) ? html : (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$centerY, has) ? A2(
					elm$html$Html$s,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class(
							A2(
								elm$core$String$join,
								' ',
								_List_fromArray(
									[mdgriffith$elm_ui$Internal$Style$classes.da, mdgriffith$elm_ui$Internal$Style$classes.e0, mdgriffith$elm_ui$Internal$Style$classes.aZ, mdgriffith$elm_ui$Internal$Style$classes.c6])))
						]),
					_List_fromArray(
						[html])) : (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$alignBottom, has) ? A2(
					elm$html$Html$u,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class(
							A2(
								elm$core$String$join,
								' ',
								_List_fromArray(
									[mdgriffith$elm_ui$Internal$Style$classes.da, mdgriffith$elm_ui$Internal$Style$classes.e0, mdgriffith$elm_ui$Internal$Style$classes.aZ, mdgriffith$elm_ui$Internal$Style$classes.c4])))
						]),
					_List_fromArray(
						[html])) : html));
			default:
				return html;
		}
	});
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var mdgriffith$elm_ui$Internal$Model$textElement = function (str) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class(
				A2(
					elm$core$String$join,
					' ',
					_List_fromArray(
						[mdgriffith$elm_ui$Internal$Style$classes.da, mdgriffith$elm_ui$Internal$Style$classes.cR, mdgriffith$elm_ui$Internal$Style$classes.bI, mdgriffith$elm_ui$Internal$Style$classes.a4])))
			]),
		_List_fromArray(
			[
				elm$html$Html$text(str)
			]));
};
var mdgriffith$elm_ui$Internal$Model$textElementFill = function (str) {
	return A3(
		elm$virtual_dom$VirtualDom$node,
		'div',
		_List_fromArray(
			[
				elm$html$Html$Attributes$class(
				A2(
					elm$core$String$join,
					' ',
					_List_fromArray(
						[mdgriffith$elm_ui$Internal$Style$classes.da, mdgriffith$elm_ui$Internal$Style$classes.cR, mdgriffith$elm_ui$Internal$Style$classes.bJ, mdgriffith$elm_ui$Internal$Style$classes.bu])))
			]),
		_List_fromArray(
			[
				elm$virtual_dom$VirtualDom$text(str)
			]));
};
var mdgriffith$elm_ui$Internal$Model$createElement = F3(
	function (context, children, rendered) {
		var gatherKeyed = F2(
			function (_n8, _n9) {
				var key = _n8.a;
				var child = _n8.b;
				var htmls = _n9.a;
				var existingStyles = _n9.b;
				switch (child.$) {
					case 0:
						var html = child.a;
						return _Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_Tuple2(
									key,
									html(context)),
								htmls),
							existingStyles);
					case 1:
						var styled = child.a;
						return _Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.d4, mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							elm$core$List$isEmpty(existingStyles) ? styled.fe : _Utils_ap(styled.fe, existingStyles)) : _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_Tuple2(
									key,
									A2(styled.d4, mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context)),
								htmls),
							elm$core$List$isEmpty(existingStyles) ? styled.fe : _Utils_ap(styled.fe, existingStyles));
					case 2:
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_Tuple2(
									key,
									_Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asEl) ? mdgriffith$elm_ui$Internal$Model$textElementFill(str) : mdgriffith$elm_ui$Internal$Model$textElement(str)),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		var gather = F2(
			function (child, _n6) {
				var htmls = _n6.a;
				var existingStyles = _n6.b;
				switch (child.$) {
					case 0:
						var html = child.a;
						return _Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								html(context),
								htmls),
							existingStyles) : _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								html(context),
								htmls),
							existingStyles);
					case 1:
						var styled = child.a;
						return _Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asParagraph) ? _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								A2(styled.d4, mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							elm$core$List$isEmpty(existingStyles) ? styled.fe : _Utils_ap(styled.fe, existingStyles)) : _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								A2(styled.d4, mdgriffith$elm_ui$Internal$Model$NoStyleSheet, context),
								htmls),
							elm$core$List$isEmpty(existingStyles) ? styled.fe : _Utils_ap(styled.fe, existingStyles));
					case 2:
						var str = child.a;
						return _Utils_Tuple2(
							A2(
								elm$core$List$cons,
								_Utils_eq(context, mdgriffith$elm_ui$Internal$Model$asEl) ? mdgriffith$elm_ui$Internal$Model$textElementFill(str) : mdgriffith$elm_ui$Internal$Model$textElement(str),
								htmls),
							existingStyles);
					default:
						return _Utils_Tuple2(htmls, existingStyles);
				}
			});
		if (children.$ === 1) {
			var keyedChildren = children.a;
			var _n1 = A3(
				elm$core$List$foldr,
				gatherKeyed,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				keyedChildren);
			var keyed = _n1.a;
			var styles = _n1.b;
			var newStyles = elm$core$List$isEmpty(styles) ? rendered.fe : _Utils_ap(rendered.fe, styles);
			if (!newStyles.b) {
				return mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.ag,
						rendered.ai,
						rendered.ac,
						mdgriffith$elm_ui$Internal$Model$Keyed(
							A3(mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.ad)),
						mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return mdgriffith$elm_ui$Internal$Model$Styled(
					{
						d4: A4(
							mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.ag,
							rendered.ai,
							rendered.ac,
							mdgriffith$elm_ui$Internal$Model$Keyed(
								A3(mdgriffith$elm_ui$Internal$Model$addKeyedChildren, 'nearby-element-pls', keyed, rendered.ad))),
						fe: allStyles
					});
			}
		} else {
			var unkeyedChildren = children.a;
			var _n3 = A3(
				elm$core$List$foldr,
				gather,
				_Utils_Tuple2(_List_Nil, _List_Nil),
				unkeyedChildren);
			var unkeyed = _n3.a;
			var styles = _n3.b;
			var newStyles = elm$core$List$isEmpty(styles) ? rendered.fe : _Utils_ap(rendered.fe, styles);
			if (!newStyles.b) {
				return mdgriffith$elm_ui$Internal$Model$Unstyled(
					A5(
						mdgriffith$elm_ui$Internal$Model$finalizeNode,
						rendered.ag,
						rendered.ai,
						rendered.ac,
						mdgriffith$elm_ui$Internal$Model$Unkeyed(
							A2(mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.ad)),
						mdgriffith$elm_ui$Internal$Model$NoStyleSheet));
			} else {
				var allStyles = newStyles;
				return mdgriffith$elm_ui$Internal$Model$Styled(
					{
						d4: A4(
							mdgriffith$elm_ui$Internal$Model$finalizeNode,
							rendered.ag,
							rendered.ai,
							rendered.ac,
							mdgriffith$elm_ui$Internal$Model$Unkeyed(
								A2(mdgriffith$elm_ui$Internal$Model$addChildren, unkeyed, rendered.ad))),
						fe: allStyles
					});
			}
		}
	});
var elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var elm$core$Bitwise$or = _Bitwise_or;
var mdgriffith$elm_ui$Internal$Flag$add = F2(
	function (myFlag, _n0) {
		var one = _n0.a;
		var two = _n0.b;
		if (!myFlag.$) {
			var first = myFlag.a;
			return A2(mdgriffith$elm_ui$Internal$Flag$Field, first | one, two);
		} else {
			var second = myFlag.a;
			return A2(mdgriffith$elm_ui$Internal$Flag$Field, one, second | two);
		}
	});
var mdgriffith$elm_ui$Internal$Flag$height = mdgriffith$elm_ui$Internal$Flag$flag(7);
var mdgriffith$elm_ui$Internal$Flag$heightContent = mdgriffith$elm_ui$Internal$Flag$flag(36);
var mdgriffith$elm_ui$Internal$Flag$merge = F2(
	function (_n0, _n1) {
		var one = _n0.a;
		var two = _n0.b;
		var three = _n1.a;
		var four = _n1.b;
		return A2(mdgriffith$elm_ui$Internal$Flag$Field, one | three, two | four);
	});
var mdgriffith$elm_ui$Internal$Flag$width = mdgriffith$elm_ui$Internal$Flag$flag(6);
var mdgriffith$elm_ui$Internal$Flag$widthContent = mdgriffith$elm_ui$Internal$Flag$flag(38);
var mdgriffith$elm_ui$Internal$Flag$xAlign = mdgriffith$elm_ui$Internal$Flag$flag(30);
var mdgriffith$elm_ui$Internal$Flag$yAlign = mdgriffith$elm_ui$Internal$Flag$flag(29);
var mdgriffith$elm_ui$Internal$Model$Embedded = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$NodeName = function (a) {
	return {$: 1, a: a};
};
var mdgriffith$elm_ui$Internal$Model$Single = F3(
	function (a, b, c) {
		return {$: 3, a: a, b: b, c: c};
	});
var mdgriffith$elm_ui$Internal$Model$Transform = function (a) {
	return {$: 10, a: a};
};
var mdgriffith$elm_ui$Internal$Model$ChildrenBehind = function (a) {
	return {$: 1, a: a};
};
var mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$ChildrenInFront = function (a) {
	return {$: 2, a: a};
};
var mdgriffith$elm_ui$Internal$Model$nearbyElement = F2(
	function (location, elem) {
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class(
					function () {
						switch (location) {
							case 0:
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.au, mdgriffith$elm_ui$Internal$Style$classes.e0, mdgriffith$elm_ui$Internal$Style$classes.c_]));
							case 1:
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.au, mdgriffith$elm_ui$Internal$Style$classes.e0, mdgriffith$elm_ui$Internal$Style$classes.dg]));
							case 2:
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.au, mdgriffith$elm_ui$Internal$Style$classes.e0, mdgriffith$elm_ui$Internal$Style$classes.ev]));
							case 3:
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.au, mdgriffith$elm_ui$Internal$Style$classes.e0, mdgriffith$elm_ui$Internal$Style$classes.et]));
							case 4:
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.au, mdgriffith$elm_ui$Internal$Style$classes.e0, mdgriffith$elm_ui$Internal$Style$classes.d8]));
							default:
								return A2(
									elm$core$String$join,
									' ',
									_List_fromArray(
										[mdgriffith$elm_ui$Internal$Style$classes.au, mdgriffith$elm_ui$Internal$Style$classes.e0, mdgriffith$elm_ui$Internal$Style$classes.df]));
						}
					}())
				]),
			_List_fromArray(
				[
					function () {
					switch (elem.$) {
						case 3:
							return elm$virtual_dom$VirtualDom$text('');
						case 2:
							var str = elem.a;
							return mdgriffith$elm_ui$Internal$Model$textElement(str);
						case 0:
							var html = elem.a;
							return html(mdgriffith$elm_ui$Internal$Model$asEl);
						default:
							var styled = elem.a;
							return A2(styled.d4, mdgriffith$elm_ui$Internal$Model$NoStyleSheet, mdgriffith$elm_ui$Internal$Model$asEl);
					}
				}()
				]));
	});
var mdgriffith$elm_ui$Internal$Model$addNearbyElement = F3(
	function (location, elem, existing) {
		var nearby = A2(mdgriffith$elm_ui$Internal$Model$nearbyElement, location, elem);
		switch (existing.$) {
			case 0:
				if (location === 5) {
					return mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						_List_fromArray(
							[nearby]));
				} else {
					return mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						_List_fromArray(
							[nearby]));
				}
			case 1:
				var existingBehind = existing.a;
				if (location === 5) {
					return mdgriffith$elm_ui$Internal$Model$ChildrenBehind(
						A2(elm$core$List$cons, nearby, existingBehind));
				} else {
					return A2(
						mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						_List_fromArray(
							[nearby]));
				}
			case 2:
				var existingInFront = existing.a;
				if (location === 5) {
					return A2(
						mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						_List_fromArray(
							[nearby]),
						existingInFront);
				} else {
					return mdgriffith$elm_ui$Internal$Model$ChildrenInFront(
						A2(elm$core$List$cons, nearby, existingInFront));
				}
			default:
				var existingBehind = existing.a;
				var existingInFront = existing.b;
				if (location === 5) {
					return A2(
						mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						A2(elm$core$List$cons, nearby, existingBehind),
						existingInFront);
				} else {
					return A2(
						mdgriffith$elm_ui$Internal$Model$ChildrenBehindAndInFront,
						existingBehind,
						A2(elm$core$List$cons, nearby, existingInFront));
				}
		}
	});
var mdgriffith$elm_ui$Internal$Model$addNodeName = F2(
	function (newNode, old) {
		switch (old.$) {
			case 0:
				return mdgriffith$elm_ui$Internal$Model$NodeName(newNode);
			case 1:
				var name = old.a;
				return A2(mdgriffith$elm_ui$Internal$Model$Embedded, name, newNode);
			default:
				var x = old.a;
				var y = old.b;
				return A2(mdgriffith$elm_ui$Internal$Model$Embedded, x, y);
		}
	});
var mdgriffith$elm_ui$Internal$Model$alignXName = function (align) {
	switch (align) {
		case 0:
			return mdgriffith$elm_ui$Internal$Style$classes.bn + (' ' + mdgriffith$elm_ui$Internal$Style$classes.bM);
		case 2:
			return mdgriffith$elm_ui$Internal$Style$classes.bn + (' ' + mdgriffith$elm_ui$Internal$Style$classes.bN);
		default:
			return mdgriffith$elm_ui$Internal$Style$classes.bn + (' ' + mdgriffith$elm_ui$Internal$Style$classes.c2);
	}
};
var mdgriffith$elm_ui$Internal$Model$alignYName = function (align) {
	switch (align) {
		case 0:
			return mdgriffith$elm_ui$Internal$Style$classes.bo + (' ' + mdgriffith$elm_ui$Internal$Style$classes.c8);
		case 2:
			return mdgriffith$elm_ui$Internal$Style$classes.bo + (' ' + mdgriffith$elm_ui$Internal$Style$classes.c1);
		default:
			return mdgriffith$elm_ui$Internal$Style$classes.bo + (' ' + mdgriffith$elm_ui$Internal$Style$classes.c3);
	}
};
var mdgriffith$elm_ui$Internal$Model$FullTransform = F4(
	function (a, b, c, d) {
		return {$: 2, a: a, b: b, c: c, d: d};
	});
var mdgriffith$elm_ui$Internal$Model$Moved = function (a) {
	return {$: 1, a: a};
};
var mdgriffith$elm_ui$Internal$Model$composeTransformation = F2(
	function (transform, component) {
		switch (transform.$) {
			case 0:
				switch (component.$) {
					case 0:
						var x = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, 0, 0));
					case 1:
						var y = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, y, 0));
					case 2:
						var z = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(0, 0, z));
					case 3:
						var xyz = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 4:
						var xyz = component.a;
						var angle = component.b;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var xyz = component.a;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(0, 0, 0),
							xyz,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			case 1:
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				switch (component.$) {
					case 0:
						var newX = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(newX, y, z));
					case 1:
						var newY = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, newY, z));
					case 2:
						var newZ = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(
							_Utils_Tuple3(x, y, newZ));
					case 3:
						var xyz = component.a;
						return mdgriffith$elm_ui$Internal$Model$Moved(xyz);
					case 4:
						var xyz = component.a;
						var angle = component.b;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							_Utils_Tuple3(1, 1, 1),
							xyz,
							angle);
					default:
						var scale = component.a;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							moved,
							scale,
							_Utils_Tuple3(0, 0, 1),
							0);
				}
			default:
				var moved = transform.a;
				var x = moved.a;
				var y = moved.b;
				var z = moved.c;
				var scaled = transform.b;
				var origin = transform.c;
				var angle = transform.d;
				switch (component.$) {
					case 0:
						var newX = component.a;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(newX, y, z),
							scaled,
							origin,
							angle);
					case 1:
						var newY = component.a;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, newY, z),
							scaled,
							origin,
							angle);
					case 2:
						var newZ = component.a;
						return A4(
							mdgriffith$elm_ui$Internal$Model$FullTransform,
							_Utils_Tuple3(x, y, newZ),
							scaled,
							origin,
							angle);
					case 3:
						var newMove = component.a;
						return A4(mdgriffith$elm_ui$Internal$Model$FullTransform, newMove, scaled, origin, angle);
					case 4:
						var newOrigin = component.a;
						var newAngle = component.b;
						return A4(mdgriffith$elm_ui$Internal$Model$FullTransform, moved, scaled, newOrigin, newAngle);
					default:
						var newScale = component.a;
						return A4(mdgriffith$elm_ui$Internal$Model$FullTransform, moved, newScale, origin, angle);
				}
		}
	});
var mdgriffith$elm_ui$Internal$Model$renderHeight = function (h) {
	switch (h.$) {
		case 0:
			var px = h.a;
			var val = elm$core$String$fromInt(px);
			var name = 'height-px-' + val;
			return _Utils_Tuple3(
				mdgriffith$elm_ui$Internal$Flag$none,
				name,
				_List_fromArray(
					[
						A3(mdgriffith$elm_ui$Internal$Model$Single, name, 'height', val + 'px')
					]));
		case 1:
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$heightContent, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.a4,
				_List_Nil);
		case 2:
			var portion = h.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$heightFill, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.bu,
				_List_Nil) : _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$heightFill, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.b6 + (' height-fill-' + elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						mdgriffith$elm_ui$Internal$Model$Single,
						mdgriffith$elm_ui$Internal$Style$classes.da + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.cH + (' > ' + mdgriffith$elm_ui$Internal$Style$dot(
							'height-fill-' + elm$core$String$fromInt(portion))))),
						'flex-grow',
						elm$core$String$fromInt(portion * 100000))
					]));
		case 3:
			var minSize = h.a;
			var len = h.b;
			var cls = 'min-height-' + elm$core$String$fromInt(minSize);
			var style = A3(
				mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-height',
				elm$core$String$fromInt(minSize) + 'px');
			var _n1 = mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _n1.a;
			var newAttrs = _n1.b;
			var newStyle = _n1.c;
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2(elm$core$List$cons, style, newStyle));
		default:
			var maxSize = h.a;
			var len = h.b;
			var cls = 'max-height-' + elm$core$String$fromInt(maxSize);
			var style = A3(
				mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-height',
				elm$core$String$fromInt(maxSize) + 'px');
			var _n2 = mdgriffith$elm_ui$Internal$Model$renderHeight(len);
			var newFlag = _n2.a;
			var newAttrs = _n2.b;
			var newStyle = _n2.c;
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$heightBetween, newFlag),
				cls + (' ' + newAttrs),
				A2(elm$core$List$cons, style, newStyle));
	}
};
var mdgriffith$elm_ui$Internal$Model$renderWidth = function (w) {
	switch (w.$) {
		case 0:
			var px = w.a;
			return _Utils_Tuple3(
				mdgriffith$elm_ui$Internal$Flag$none,
				mdgriffith$elm_ui$Internal$Style$classes.cX + (' width-px-' + elm$core$String$fromInt(px)),
				_List_fromArray(
					[
						A3(
						mdgriffith$elm_ui$Internal$Model$Single,
						'width-px-' + elm$core$String$fromInt(px),
						'width',
						elm$core$String$fromInt(px) + 'px')
					]));
		case 1:
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$widthContent, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.bI,
				_List_Nil);
		case 2:
			var portion = w.a;
			return (portion === 1) ? _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$widthFill, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.bJ,
				_List_Nil) : _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$widthFill, mdgriffith$elm_ui$Internal$Flag$none),
				mdgriffith$elm_ui$Internal$Style$classes.cY + (' width-fill-' + elm$core$String$fromInt(portion)),
				_List_fromArray(
					[
						A3(
						mdgriffith$elm_ui$Internal$Model$Single,
						mdgriffith$elm_ui$Internal$Style$classes.da + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.cH + (' > ' + mdgriffith$elm_ui$Internal$Style$dot(
							'width-fill-' + elm$core$String$fromInt(portion))))),
						'flex-grow',
						elm$core$String$fromInt(portion * 100000))
					]));
		case 3:
			var minSize = w.a;
			var len = w.b;
			var cls = 'min-width-' + elm$core$String$fromInt(minSize);
			var style = A3(
				mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'min-width',
				elm$core$String$fromInt(minSize) + 'px');
			var _n1 = mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _n1.a;
			var newAttrs = _n1.b;
			var newStyle = _n1.c;
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2(elm$core$List$cons, style, newStyle));
		default:
			var maxSize = w.a;
			var len = w.b;
			var cls = 'max-width-' + elm$core$String$fromInt(maxSize);
			var style = A3(
				mdgriffith$elm_ui$Internal$Model$Single,
				cls,
				'max-width',
				elm$core$String$fromInt(maxSize) + 'px');
			var _n2 = mdgriffith$elm_ui$Internal$Model$renderWidth(len);
			var newFlag = _n2.a;
			var newAttrs = _n2.b;
			var newStyle = _n2.c;
			return _Utils_Tuple3(
				A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$widthBetween, newFlag),
				cls + (' ' + newAttrs),
				A2(elm$core$List$cons, style, newStyle));
	}
};
var mdgriffith$elm_ui$Internal$Flag$borderWidth = mdgriffith$elm_ui$Internal$Flag$flag(27);
var mdgriffith$elm_ui$Internal$Model$skippable = F2(
	function (flag, style) {
		if (_Utils_eq(flag, mdgriffith$elm_ui$Internal$Flag$borderWidth)) {
			if (style.$ === 3) {
				var val = style.c;
				switch (val) {
					case '0px':
						return true;
					case '1px':
						return true;
					case '2px':
						return true;
					case '3px':
						return true;
					case '4px':
						return true;
					case '5px':
						return true;
					case '6px':
						return true;
					default:
						return false;
				}
			} else {
				return false;
			}
		} else {
			switch (style.$) {
				case 2:
					var i = style.a;
					return (i >= 8) && (i <= 32);
				case 7:
					var name = style.a;
					var t = style.b;
					var r = style.c;
					var b = style.d;
					var l = style.e;
					return _Utils_eq(t, b) && (_Utils_eq(t, r) && (_Utils_eq(t, l) && ((t >= 0) && (t <= 24))));
				default:
					return false;
			}
		}
	});
var mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive = F8(
	function (classes, node, has, transform, styles, attrs, children, elementAttrs) {
		gatherAttrRecursive:
		while (true) {
			if (!elementAttrs.b) {
				var _n1 = mdgriffith$elm_ui$Internal$Model$transformClass(transform);
				if (_n1.$ === 1) {
					return {
						ac: A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$class(classes),
							attrs),
						ad: children,
						ag: has,
						ai: node,
						fe: styles
					};
				} else {
					var _class = _n1.a;
					return {
						ac: A2(
							elm$core$List$cons,
							elm$html$Html$Attributes$class(classes + (' ' + _class)),
							attrs),
						ad: children,
						ag: has,
						ai: node,
						fe: A2(
							elm$core$List$cons,
							mdgriffith$elm_ui$Internal$Model$Transform(transform),
							styles)
					};
				}
			} else {
				var attribute = elementAttrs.a;
				var remaining = elementAttrs.b;
				switch (attribute.$) {
					case 0:
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 3:
						var flag = attribute.a;
						var exactClassName = attribute.b;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = exactClassName + (' ' + classes),
								$temp$node = node,
								$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, flag, has),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					case 1:
						var actualAttribute = attribute.a;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = styles,
							$temp$attrs = A2(elm$core$List$cons, actualAttribute, attrs),
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 4:
						var flag = attribute.a;
						var style = attribute.b;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, flag, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							if (A2(mdgriffith$elm_ui$Internal$Model$skippable, flag, style)) {
								var $temp$classes = mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							} else {
								var $temp$classes = mdgriffith$elm_ui$Internal$Model$getStyleName(style) + (' ' + classes),
									$temp$node = node,
									$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, flag, has),
									$temp$transform = transform,
									$temp$styles = A2(elm$core$List$cons, style, styles),
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							}
						}
					case 10:
						var flag = attribute.a;
						var component = attribute.b;
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, flag, has),
							$temp$transform = A2(mdgriffith$elm_ui$Internal$Model$composeTransformation, transform, component),
							$temp$styles = styles,
							$temp$attrs = attrs,
							$temp$children = children,
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 7:
						var width = attribute.a;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$width, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (width.$) {
								case 0:
									var px = width.a;
									var $temp$classes = (mdgriffith$elm_ui$Internal$Style$classes.cX + (' width-px-' + elm$core$String$fromInt(px))) + (' ' + classes),
										$temp$node = node,
										$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$width, has),
										$temp$transform = transform,
										$temp$styles = A2(
										elm$core$List$cons,
										A3(
											mdgriffith$elm_ui$Internal$Model$Single,
											'width-px-' + elm$core$String$fromInt(px),
											'width',
											elm$core$String$fromInt(px) + 'px'),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 1:
									var $temp$classes = classes + (' ' + mdgriffith$elm_ui$Internal$Style$classes.bI),
										$temp$node = node,
										$temp$has = A2(
										mdgriffith$elm_ui$Internal$Flag$add,
										mdgriffith$elm_ui$Internal$Flag$widthContent,
										A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$width, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 2:
									var portion = width.a;
									if (portion === 1) {
										var $temp$classes = classes + (' ' + mdgriffith$elm_ui$Internal$Style$classes.bJ),
											$temp$node = node,
											$temp$has = A2(
											mdgriffith$elm_ui$Internal$Flag$add,
											mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + (mdgriffith$elm_ui$Internal$Style$classes.cY + (' width-fill-' + elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											mdgriffith$elm_ui$Internal$Flag$add,
											mdgriffith$elm_ui$Internal$Flag$widthFill,
											A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$width, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											elm$core$List$cons,
											A3(
												mdgriffith$elm_ui$Internal$Model$Single,
												mdgriffith$elm_ui$Internal$Style$classes.da + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.cH + (' > ' + mdgriffith$elm_ui$Internal$Style$dot(
													'width-fill-' + elm$core$String$fromInt(portion))))),
												'flex-grow',
												elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _n4 = mdgriffith$elm_ui$Internal$Model$renderWidth(width);
									var addToFlags = _n4.a;
									var newClass = _n4.b;
									var newStyles = _n4.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$merge, addToFlags, has),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 8:
						var height = attribute.a;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$height, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							switch (height.$) {
								case 0:
									var px = height.a;
									var val = elm$core$String$fromInt(px) + 'px';
									var name = 'height-px-' + val;
									var $temp$classes = name + (' ' + classes),
										$temp$node = node,
										$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$height, has),
										$temp$transform = transform,
										$temp$styles = A2(
										elm$core$List$cons,
										A3(mdgriffith$elm_ui$Internal$Model$Single, name, 'height ', val),
										styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 1:
									var $temp$classes = mdgriffith$elm_ui$Internal$Style$classes.a4 + (' ' + classes),
										$temp$node = node,
										$temp$has = A2(
										mdgriffith$elm_ui$Internal$Flag$add,
										mdgriffith$elm_ui$Internal$Flag$heightContent,
										A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$height, has)),
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								case 2:
									var portion = height.a;
									if (portion === 1) {
										var $temp$classes = mdgriffith$elm_ui$Internal$Style$classes.bu + (' ' + classes),
											$temp$node = node,
											$temp$has = A2(
											mdgriffith$elm_ui$Internal$Flag$add,
											mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes + (' ' + (mdgriffith$elm_ui$Internal$Style$classes.b6 + (' height-fill-' + elm$core$String$fromInt(portion)))),
											$temp$node = node,
											$temp$has = A2(
											mdgriffith$elm_ui$Internal$Flag$add,
											mdgriffith$elm_ui$Internal$Flag$heightFill,
											A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$height, has)),
											$temp$transform = transform,
											$temp$styles = A2(
											elm$core$List$cons,
											A3(
												mdgriffith$elm_ui$Internal$Model$Single,
												mdgriffith$elm_ui$Internal$Style$classes.da + ('.' + (mdgriffith$elm_ui$Internal$Style$classes.S + (' > ' + mdgriffith$elm_ui$Internal$Style$dot(
													'height-fill-' + elm$core$String$fromInt(portion))))),
												'flex-grow',
												elm$core$String$fromInt(portion * 100000)),
											styles),
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								default:
									var _n6 = mdgriffith$elm_ui$Internal$Model$renderHeight(height);
									var addToFlags = _n6.a;
									var newClass = _n6.b;
									var newStyles = _n6.c;
									var $temp$classes = classes + (' ' + newClass),
										$temp$node = node,
										$temp$has = A2(mdgriffith$elm_ui$Internal$Flag$merge, addToFlags, has),
										$temp$transform = transform,
										$temp$styles = _Utils_ap(newStyles, styles),
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
							}
						}
					case 2:
						var description = attribute.a;
						switch (description.$) {
							case 0:
								var $temp$classes = classes,
									$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'main', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 1:
								var $temp$classes = classes,
									$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'nav', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 2:
								var $temp$classes = classes,
									$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'footer', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 3:
								var $temp$classes = classes,
									$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'aside', node),
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 4:
								var i = description.a;
								if (i <= 1) {
									var $temp$classes = classes,
										$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'h1', node),
										$temp$has = has,
										$temp$transform = transform,
										$temp$styles = styles,
										$temp$attrs = attrs,
										$temp$children = children,
										$temp$elementAttrs = remaining;
									classes = $temp$classes;
									node = $temp$node;
									has = $temp$has;
									transform = $temp$transform;
									styles = $temp$styles;
									attrs = $temp$attrs;
									children = $temp$children;
									elementAttrs = $temp$elementAttrs;
									continue gatherAttrRecursive;
								} else {
									if (i < 7) {
										var $temp$classes = classes,
											$temp$node = A2(
											mdgriffith$elm_ui$Internal$Model$addNodeName,
											'h' + elm$core$String$fromInt(i),
											node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									} else {
										var $temp$classes = classes,
											$temp$node = A2(mdgriffith$elm_ui$Internal$Model$addNodeName, 'h6', node),
											$temp$has = has,
											$temp$transform = transform,
											$temp$styles = styles,
											$temp$attrs = attrs,
											$temp$children = children,
											$temp$elementAttrs = remaining;
										classes = $temp$classes;
										node = $temp$node;
										has = $temp$has;
										transform = $temp$transform;
										styles = $temp$styles;
										attrs = $temp$attrs;
										children = $temp$children;
										elementAttrs = $temp$elementAttrs;
										continue gatherAttrRecursive;
									}
								}
							case 9:
								var newNode = function () {
									switch (node.$) {
										case 0:
											return mdgriffith$elm_ui$Internal$Model$NodeName('p');
										case 1:
											var name = node.a;
											return mdgriffith$elm_ui$Internal$Model$NodeName(name);
										default:
											var x = node.a;
											var y = node.b;
											return A2(mdgriffith$elm_ui$Internal$Model$Embedded, x, y);
									}
								}();
								var $temp$classes = classes,
									$temp$node = newNode,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = attrs,
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 8:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									elm$core$List$cons,
									A2(elm$virtual_dom$VirtualDom$attribute, 'role', 'button'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 5:
								var label = description.a;
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									elm$core$List$cons,
									A2(elm$virtual_dom$VirtualDom$attribute, 'aria-label', label),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							case 6:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									elm$core$List$cons,
									A2(elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'polite'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
							default:
								var $temp$classes = classes,
									$temp$node = node,
									$temp$has = has,
									$temp$transform = transform,
									$temp$styles = styles,
									$temp$attrs = A2(
									elm$core$List$cons,
									A2(elm$virtual_dom$VirtualDom$attribute, 'aria-live', 'assertive'),
									attrs),
									$temp$children = children,
									$temp$elementAttrs = remaining;
								classes = $temp$classes;
								node = $temp$node;
								has = $temp$has;
								transform = $temp$transform;
								styles = $temp$styles;
								attrs = $temp$attrs;
								children = $temp$children;
								elementAttrs = $temp$elementAttrs;
								continue gatherAttrRecursive;
						}
					case 9:
						var location = attribute.a;
						var elem = attribute.b;
						var newStyles = function () {
							switch (elem.$) {
								case 3:
									return styles;
								case 2:
									var str = elem.a;
									return styles;
								case 0:
									var html = elem.a;
									return styles;
								default:
									var styled = elem.a;
									return _Utils_ap(styles, styled.fe);
							}
						}();
						var $temp$classes = classes,
							$temp$node = node,
							$temp$has = has,
							$temp$transform = transform,
							$temp$styles = newStyles,
							$temp$attrs = attrs,
							$temp$children = A3(mdgriffith$elm_ui$Internal$Model$addNearbyElement, location, elem, children),
							$temp$elementAttrs = remaining;
						classes = $temp$classes;
						node = $temp$node;
						has = $temp$has;
						transform = $temp$transform;
						styles = $temp$styles;
						attrs = $temp$attrs;
						children = $temp$children;
						elementAttrs = $temp$elementAttrs;
						continue gatherAttrRecursive;
					case 6:
						var x = attribute.a;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$xAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = mdgriffith$elm_ui$Internal$Model$alignXName(x) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (x) {
									case 1:
										return A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$centerX, flags);
									case 2:
										return A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$alignRight, flags);
									default:
										return flags;
								}
							}(
								A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$xAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
					default:
						var y = attribute.a;
						if (A2(mdgriffith$elm_ui$Internal$Flag$present, mdgriffith$elm_ui$Internal$Flag$yAlign, has)) {
							var $temp$classes = classes,
								$temp$node = node,
								$temp$has = has,
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						} else {
							var $temp$classes = mdgriffith$elm_ui$Internal$Model$alignYName(y) + (' ' + classes),
								$temp$node = node,
								$temp$has = function (flags) {
								switch (y) {
									case 1:
										return A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$centerY, flags);
									case 2:
										return A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$alignBottom, flags);
									default:
										return flags;
								}
							}(
								A2(mdgriffith$elm_ui$Internal$Flag$add, mdgriffith$elm_ui$Internal$Flag$yAlign, has)),
								$temp$transform = transform,
								$temp$styles = styles,
								$temp$attrs = attrs,
								$temp$children = children,
								$temp$elementAttrs = remaining;
							classes = $temp$classes;
							node = $temp$node;
							has = $temp$has;
							transform = $temp$transform;
							styles = $temp$styles;
							attrs = $temp$attrs;
							children = $temp$children;
							elementAttrs = $temp$elementAttrs;
							continue gatherAttrRecursive;
						}
				}
			}
		}
	});
var mdgriffith$elm_ui$Internal$Model$Untransformed = {$: 0};
var mdgriffith$elm_ui$Internal$Model$untransformed = mdgriffith$elm_ui$Internal$Model$Untransformed;
var mdgriffith$elm_ui$Internal$Model$element = F4(
	function (context, node, attributes, children) {
		return A3(
			mdgriffith$elm_ui$Internal$Model$createElement,
			context,
			children,
			A8(
				mdgriffith$elm_ui$Internal$Model$gatherAttrRecursive,
				mdgriffith$elm_ui$Internal$Model$contextClasses(context),
				node,
				mdgriffith$elm_ui$Internal$Flag$none,
				mdgriffith$elm_ui$Internal$Model$untransformed,
				_List_Nil,
				_List_Nil,
				mdgriffith$elm_ui$Internal$Model$NoNearbyChildren,
				elm$core$List$reverse(attributes)));
	});
var mdgriffith$elm_ui$Element$el = F2(
	function (attrs, child) {
		return A4(
			mdgriffith$elm_ui$Internal$Model$element,
			mdgriffith$elm_ui$Internal$Model$asEl,
			mdgriffith$elm_ui$Internal$Model$div,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$shrink),
				A2(
					elm$core$List$cons,
					mdgriffith$elm_ui$Element$height(mdgriffith$elm_ui$Element$shrink),
					attrs)),
			mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[child])));
	});
var mdgriffith$elm_ui$Internal$Flag$hover = mdgriffith$elm_ui$Internal$Flag$flag(33);
var mdgriffith$elm_ui$Internal$Model$Hover = 1;
var mdgriffith$elm_ui$Internal$Model$PseudoSelector = F2(
	function (a, b) {
		return {$: 11, a: a, b: b};
	});
var elm$virtual_dom$VirtualDom$mapAttribute = _VirtualDom_mapAttribute;
var mdgriffith$elm_ui$Internal$Model$Attr = function (a) {
	return {$: 1, a: a};
};
var mdgriffith$elm_ui$Internal$Model$Describe = function (a) {
	return {$: 2, a: a};
};
var mdgriffith$elm_ui$Internal$Model$Nearby = F2(
	function (a, b) {
		return {$: 9, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$NoAttribute = {$: 0};
var mdgriffith$elm_ui$Internal$Model$TransformComponent = F2(
	function (a, b) {
		return {$: 10, a: a, b: b};
	});
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var mdgriffith$elm_ui$Internal$Model$Empty = {$: 3};
var mdgriffith$elm_ui$Internal$Model$map = F2(
	function (fn, el) {
		switch (el.$) {
			case 1:
				var styled = el.a;
				return mdgriffith$elm_ui$Internal$Model$Styled(
					{
						d4: F2(
							function (add, context) {
								return A2(
									elm$virtual_dom$VirtualDom$map,
									fn,
									A2(styled.d4, add, context));
							}),
						fe: styled.fe
					});
			case 0:
				var html = el.a;
				return mdgriffith$elm_ui$Internal$Model$Unstyled(
					A2(
						elm$core$Basics$composeL,
						elm$virtual_dom$VirtualDom$map(fn),
						html));
			case 2:
				var str = el.a;
				return mdgriffith$elm_ui$Internal$Model$Text(str);
			default:
				return mdgriffith$elm_ui$Internal$Model$Empty;
		}
	});
var mdgriffith$elm_ui$Internal$Model$mapAttrFromStyle = F2(
	function (fn, attr) {
		switch (attr.$) {
			case 0:
				return mdgriffith$elm_ui$Internal$Model$NoAttribute;
			case 2:
				var description = attr.a;
				return mdgriffith$elm_ui$Internal$Model$Describe(description);
			case 6:
				var x = attr.a;
				return mdgriffith$elm_ui$Internal$Model$AlignX(x);
			case 5:
				var y = attr.a;
				return mdgriffith$elm_ui$Internal$Model$AlignY(y);
			case 7:
				var x = attr.a;
				return mdgriffith$elm_ui$Internal$Model$Width(x);
			case 8:
				var x = attr.a;
				return mdgriffith$elm_ui$Internal$Model$Height(x);
			case 3:
				var x = attr.a;
				var y = attr.b;
				return A2(mdgriffith$elm_ui$Internal$Model$Class, x, y);
			case 4:
				var flag = attr.a;
				var style = attr.b;
				return A2(mdgriffith$elm_ui$Internal$Model$StyleClass, flag, style);
			case 9:
				var location = attr.a;
				var elem = attr.b;
				return A2(
					mdgriffith$elm_ui$Internal$Model$Nearby,
					location,
					A2(mdgriffith$elm_ui$Internal$Model$map, fn, elem));
			case 1:
				var htmlAttr = attr.a;
				return mdgriffith$elm_ui$Internal$Model$Attr(
					A2(elm$virtual_dom$VirtualDom$mapAttribute, fn, htmlAttr));
			default:
				var fl = attr.a;
				var trans = attr.b;
				return A2(mdgriffith$elm_ui$Internal$Model$TransformComponent, fl, trans);
		}
	});
var mdgriffith$elm_ui$Internal$Model$removeNever = function (style) {
	return A2(mdgriffith$elm_ui$Internal$Model$mapAttrFromStyle, elm$core$Basics$never, style);
};
var mdgriffith$elm_ui$Internal$Model$unwrapDecsHelper = F2(
	function (attr, _n0) {
		var styles = _n0.a;
		var trans = _n0.b;
		var _n1 = mdgriffith$elm_ui$Internal$Model$removeNever(attr);
		switch (_n1.$) {
			case 4:
				var style = _n1.b;
				return _Utils_Tuple2(
					A2(elm$core$List$cons, style, styles),
					trans);
			case 10:
				var flag = _n1.a;
				var component = _n1.b;
				return _Utils_Tuple2(
					styles,
					A2(mdgriffith$elm_ui$Internal$Model$composeTransformation, trans, component));
			default:
				return _Utils_Tuple2(styles, trans);
		}
	});
var mdgriffith$elm_ui$Internal$Model$unwrapDecorations = function (attrs) {
	var _n0 = A3(
		elm$core$List$foldl,
		mdgriffith$elm_ui$Internal$Model$unwrapDecsHelper,
		_Utils_Tuple2(_List_Nil, mdgriffith$elm_ui$Internal$Model$Untransformed),
		attrs);
	var styles = _n0.a;
	var transform = _n0.b;
	return A2(
		elm$core$List$cons,
		mdgriffith$elm_ui$Internal$Model$Transform(transform),
		styles);
};
var mdgriffith$elm_ui$Element$mouseOver = function (decs) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$hover,
		A2(
			mdgriffith$elm_ui$Internal$Model$PseudoSelector,
			1,
			mdgriffith$elm_ui$Internal$Model$unwrapDecorations(decs)));
};
var mdgriffith$elm_ui$Element$none = mdgriffith$elm_ui$Internal$Model$Empty;
var mdgriffith$elm_ui$Internal$Model$AsRow = 0;
var mdgriffith$elm_ui$Internal$Model$asRow = 0;
var mdgriffith$elm_ui$Internal$Model$htmlClass = function (cls) {
	return mdgriffith$elm_ui$Internal$Model$Attr(
		elm$html$Html$Attributes$class(cls));
};
var mdgriffith$elm_ui$Element$row = F2(
	function (attrs, children) {
		return A4(
			mdgriffith$elm_ui$Internal$Model$element,
			mdgriffith$elm_ui$Internal$Model$asRow,
			mdgriffith$elm_ui$Internal$Model$div,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Internal$Model$htmlClass(mdgriffith$elm_ui$Internal$Style$classes.aG + (' ' + mdgriffith$elm_ui$Internal$Style$classes.T)),
				A2(
					elm$core$List$cons,
					mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$shrink),
					A2(
						elm$core$List$cons,
						mdgriffith$elm_ui$Element$height(mdgriffith$elm_ui$Element$shrink),
						attrs))),
			mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var elm$json$Json$Encode$bool = _Json_wrap;
var elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$bool(bool));
	});
var elm$html$Html$Attributes$disabled = elm$html$Html$Attributes$boolProperty('disabled');
var elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		elm$core$String$fromInt(n));
};
var mdgriffith$elm_ui$Internal$Flag$cursor = mdgriffith$elm_ui$Internal$Flag$flag(21);
var mdgriffith$elm_ui$Element$pointer = A2(mdgriffith$elm_ui$Internal$Model$Class, mdgriffith$elm_ui$Internal$Flag$cursor, mdgriffith$elm_ui$Internal$Style$classes.dG);
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var mdgriffith$elm_ui$Element$Events$onClick = A2(elm$core$Basics$composeL, mdgriffith$elm_ui$Internal$Model$Attr, elm$html$Html$Events$onClick);
var mdgriffith$elm_ui$Element$Input$hasFocusStyle = function (attr) {
	if (((attr.$ === 4) && (attr.b.$ === 11)) && (!attr.b.a)) {
		var _n1 = attr.b;
		var _n2 = _n1.a;
		return true;
	} else {
		return false;
	}
};
var mdgriffith$elm_ui$Element$Input$focusDefault = function (attrs) {
	return A2(elm$core$List$any, mdgriffith$elm_ui$Element$Input$hasFocusStyle, attrs) ? mdgriffith$elm_ui$Internal$Model$NoAttribute : mdgriffith$elm_ui$Internal$Model$htmlClass('focusable');
};
var mdgriffith$elm_ui$Element$Input$enter = 'Enter';
var elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 2, a: a};
};
var elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var mdgriffith$elm_ui$Element$Input$onKey = F2(
	function (desiredCode, msg) {
		var decode = function (code) {
			return _Utils_eq(code, desiredCode) ? elm$json$Json$Decode$succeed(msg) : elm$json$Json$Decode$fail('Not the enter key');
		};
		var isKey = A2(
			elm$json$Json$Decode$andThen,
			decode,
			A2(elm$json$Json$Decode$field, 'key', elm$json$Json$Decode$string));
		return mdgriffith$elm_ui$Internal$Model$Attr(
			A2(
				elm$html$Html$Events$preventDefaultOn,
				'keyup',
				A2(
					elm$json$Json$Decode$map,
					function (fired) {
						return _Utils_Tuple2(fired, true);
					},
					isKey)));
	});
var mdgriffith$elm_ui$Element$Input$onEnter = function (msg) {
	return A2(mdgriffith$elm_ui$Element$Input$onKey, mdgriffith$elm_ui$Element$Input$enter, msg);
};
var mdgriffith$elm_ui$Internal$Model$Button = {$: 8};
var mdgriffith$elm_ui$Element$Input$button = F2(
	function (attrs, _n0) {
		var onPress = _n0.eu;
		var label = _n0.ef;
		return A4(
			mdgriffith$elm_ui$Internal$Model$element,
			mdgriffith$elm_ui$Internal$Model$asEl,
			mdgriffith$elm_ui$Internal$Model$div,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$shrink),
				A2(
					elm$core$List$cons,
					mdgriffith$elm_ui$Element$height(mdgriffith$elm_ui$Element$shrink),
					A2(
						elm$core$List$cons,
						mdgriffith$elm_ui$Internal$Model$htmlClass(mdgriffith$elm_ui$Internal$Style$classes.a$ + (' ' + (mdgriffith$elm_ui$Internal$Style$classes.T + (' ' + (mdgriffith$elm_ui$Internal$Style$classes.eU + (' ' + mdgriffith$elm_ui$Internal$Style$classes.ck)))))),
						A2(
							elm$core$List$cons,
							mdgriffith$elm_ui$Element$pointer,
							A2(
								elm$core$List$cons,
								mdgriffith$elm_ui$Element$Input$focusDefault(attrs),
								A2(
									elm$core$List$cons,
									mdgriffith$elm_ui$Internal$Model$Describe(mdgriffith$elm_ui$Internal$Model$Button),
									A2(
										elm$core$List$cons,
										mdgriffith$elm_ui$Internal$Model$Attr(
											elm$html$Html$Attributes$tabindex(0)),
										function () {
											if (onPress.$ === 1) {
												return A2(
													elm$core$List$cons,
													mdgriffith$elm_ui$Internal$Model$Attr(
														elm$html$Html$Attributes$disabled(true)),
													attrs);
											} else {
												var msg = onPress.a;
												return A2(
													elm$core$List$cons,
													mdgriffith$elm_ui$Element$Events$onClick(msg),
													A2(
														elm$core$List$cons,
														mdgriffith$elm_ui$Element$Input$onEnter(msg),
														attrs));
											}
										}()))))))),
			mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[label])));
	});
var author$project$BusinessFunctions$Pager$buildPageNumbersView = F2(
	function (pages, currentPageNumber) {
		var searchStringBtnStyle = function (num) {
			return _Utils_eq(currentPageNumber, num) ? _List_fromArray(
				[
					author$project$Helpers$Colors$greyBg(185),
					author$project$Helpers$Colors$greyFont(57),
					author$project$Helpers$ElmStyleShotcuts$fs(16)
				]) : _List_fromArray(
				[
					author$project$Helpers$Colors$mhcMediumRed,
					author$project$Helpers$ElmStyleShotcuts$fs(12)
				]);
		};
		var pageNumbers = A2(elm$core$List$range, 1, pages);
		return (pages > 1) ? A2(
			elm$core$List$map,
			function (num) {
				return A2(
					mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[
							author$project$Helpers$ElmStyleShotcuts$wpx(25),
							author$project$Helpers$ElmStyleShotcuts$hpx(20)
						]),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Element$Input$button,
							_Utils_ap(
								_List_fromArray(
									[
										(!_Utils_eq(currentPageNumber, num)) ? mdgriffith$elm_ui$Element$mouseOver(
										_List_fromArray(
											[
												author$project$Helpers$Colors$greyBg(0),
												author$project$Helpers$Colors$greyFont(250)
											])) : mdgriffith$elm_ui$Element$mouseOver(
										_List_fromArray(
											[
												author$project$Helpers$Colors$greyBg(175)
											])),
										author$project$Helpers$ElmStyleShotcuts$wf,
										author$project$Helpers$ElmStyleShotcuts$hf,
										author$project$Helpers$ElmStyleShotcuts$fb
									]),
								searchStringBtnStyle(num)),
							{
								ef: A2(
									mdgriffith$elm_ui$Element$el,
									_List_fromArray(
										[author$project$Helpers$ElmStyleShotcuts$eacx, author$project$Helpers$ElmStyleShotcuts$eacy]),
									author$project$Helpers$ElmUI$textValue(
										elm$core$String$fromInt(num))),
								eu: elm$core$Maybe$Just(
									author$project$Msg$PageNumberClicked(num))
							})
						]));
			},
			pageNumbers) : _List_fromArray(
			[mdgriffith$elm_ui$Element$none]);
	});
var author$project$BusinessFunctions$SearchFilterFunctions$anyFilterApplied = function (uiModel) {
	return elm$core$List$length(uiModel.bD) > 0;
};
var mdgriffith$elm_ui$Internal$Flag$borderColor = mdgriffith$elm_ui$Internal$Flag$flag(28);
var mdgriffith$elm_ui$Element$Border$color = function (clr) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$borderColor,
		A3(
			mdgriffith$elm_ui$Internal$Model$Colored,
			'bc-' + mdgriffith$elm_ui$Internal$Model$formatColorClass(clr),
			'border-color',
			clr));
};
var author$project$Helpers$ElmStyleShotcuts$brc = F3(
	function (red, green, blue) {
		return mdgriffith$elm_ui$Element$Border$color(
			A3(mdgriffith$elm_ui$Element$rgb255, red, green, blue));
	});
var author$project$Helpers$Colors$greyBorder = function (num) {
	return A3(author$project$Helpers$ElmStyleShotcuts$brc, num, num, num);
};
var author$project$Helpers$Colors$mhcRed = A3(author$project$Helpers$ElmStyleShotcuts$fc, 190, 5, 30);
var mdgriffith$elm_ui$Internal$Model$BorderWidth = F5(
	function (a, b, c, d, e) {
		return {$: 6, a: a, b: b, c: c, d: d, e: e};
	});
var mdgriffith$elm_ui$Element$Border$width = function (v) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$borderWidth,
		A5(
			mdgriffith$elm_ui$Internal$Model$BorderWidth,
			'b-' + elm$core$String$fromInt(v),
			v,
			v,
			v,
			v));
};
var author$project$Helpers$ElmStyleShotcuts$bw = function (number) {
	return mdgriffith$elm_ui$Element$Border$width(number);
};
var author$project$Helpers$ElmStyleShotcuts$edges = {aq: 0, at: 0, az: 0, aB: 0};
var mdgriffith$elm_ui$Element$Border$widthXY = F2(
	function (x, y) {
		return A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$borderWidth,
			A5(
				mdgriffith$elm_ui$Internal$Model$BorderWidth,
				'b-' + (elm$core$String$fromInt(x) + ('-' + elm$core$String$fromInt(y))),
				y,
				x,
				y,
				x));
	});
var mdgriffith$elm_ui$Element$Border$widthEach = function (_n0) {
	var bottom = _n0.aq;
	var top = _n0.aB;
	var left = _n0.at;
	var right = _n0.az;
	return (_Utils_eq(top, bottom) && _Utils_eq(left, right)) ? (_Utils_eq(top, right) ? mdgriffith$elm_ui$Element$Border$width(top) : A2(mdgriffith$elm_ui$Element$Border$widthXY, left, top)) : A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$borderWidth,
		A5(
			mdgriffith$elm_ui$Internal$Model$BorderWidth,
			'b-' + (elm$core$String$fromInt(top) + ('-' + (elm$core$String$fromInt(right) + ('-' + (elm$core$String$fromInt(bottom) + ('-' + elm$core$String$fromInt(left))))))),
			top,
			right,
			bottom,
			left));
};
var author$project$Helpers$ElmStyleShotcuts$bwb = function (number) {
	return mdgriffith$elm_ui$Element$Border$widthEach(
		_Utils_update(
			author$project$Helpers$ElmStyleShotcuts$edges,
			{aq: number}));
};
var author$project$Helpers$ElmStyleShotcuts$bwl = function (number) {
	return mdgriffith$elm_ui$Element$Border$widthEach(
		_Utils_update(
			author$project$Helpers$ElmStyleShotcuts$edges,
			{at: number}));
};
var author$project$Helpers$ElmStyleShotcuts$bwr = function (number) {
	return mdgriffith$elm_ui$Element$Border$widthEach(
		_Utils_update(
			author$project$Helpers$ElmStyleShotcuts$edges,
			{az: number}));
};
var mdgriffith$elm_ui$Internal$Model$Bottom = 2;
var mdgriffith$elm_ui$Element$alignBottom = mdgriffith$elm_ui$Internal$Model$AlignY(2);
var author$project$Helpers$ElmStyleShotcuts$eab = mdgriffith$elm_ui$Element$alignBottom;
var mdgriffith$elm_ui$Internal$Model$Right = 2;
var mdgriffith$elm_ui$Element$alignRight = mdgriffith$elm_ui$Internal$Model$AlignX(2);
var author$project$Helpers$ElmStyleShotcuts$ear = mdgriffith$elm_ui$Element$alignRight;
var mdgriffith$elm_ui$Internal$Model$Top = 0;
var mdgriffith$elm_ui$Element$alignTop = mdgriffith$elm_ui$Internal$Model$AlignY(0);
var author$project$Helpers$ElmStyleShotcuts$eat = mdgriffith$elm_ui$Element$alignTop;
var mdgriffith$elm_ui$Internal$Model$Max = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var mdgriffith$elm_ui$Element$maximum = F2(
	function (i, l) {
		return A2(mdgriffith$elm_ui$Internal$Model$Max, i, l);
	});
var mdgriffith$elm_ui$Internal$Model$Min = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var mdgriffith$elm_ui$Element$minimum = F2(
	function (i, l) {
		return A2(mdgriffith$elm_ui$Internal$Model$Min, i, l);
	});
var author$project$Helpers$ElmStyleShotcuts$hfRange = F2(
	function (minValue, maxValue) {
		return mdgriffith$elm_ui$Element$height(
			A2(
				mdgriffith$elm_ui$Element$maximum,
				maxValue,
				A2(mdgriffith$elm_ui$Element$minimum, minValue, mdgriffith$elm_ui$Element$fill)));
	});
var mdgriffith$elm_ui$Internal$Flag$padding = mdgriffith$elm_ui$Internal$Flag$flag(2);
var mdgriffith$elm_ui$Internal$Model$PaddingStyle = F5(
	function (a, b, c, d, e) {
		return {$: 7, a: a, b: b, c: c, d: d, e: e};
	});
var mdgriffith$elm_ui$Element$padding = function (x) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$padding,
		A5(
			mdgriffith$elm_ui$Internal$Model$PaddingStyle,
			'p-' + elm$core$String$fromInt(x),
			x,
			x,
			x,
			x));
};
var author$project$Helpers$ElmStyleShotcuts$pd = function (number) {
	return mdgriffith$elm_ui$Element$padding(number);
};
var mdgriffith$elm_ui$Internal$Model$paddingName = F4(
	function (top, right, bottom, left) {
		return 'pad-' + (elm$core$String$fromInt(top) + ('-' + (elm$core$String$fromInt(right) + ('-' + (elm$core$String$fromInt(bottom) + ('-' + elm$core$String$fromInt(left)))))));
	});
var mdgriffith$elm_ui$Element$paddingEach = function (_n0) {
	var top = _n0.aB;
	var right = _n0.az;
	var bottom = _n0.aq;
	var left = _n0.at;
	return (_Utils_eq(top, right) && (_Utils_eq(top, bottom) && _Utils_eq(top, left))) ? A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$padding,
		A5(
			mdgriffith$elm_ui$Internal$Model$PaddingStyle,
			'p-' + elm$core$String$fromInt(top),
			top,
			top,
			top,
			top)) : A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$padding,
		A5(
			mdgriffith$elm_ui$Internal$Model$PaddingStyle,
			A4(mdgriffith$elm_ui$Internal$Model$paddingName, top, right, bottom, left),
			top,
			right,
			bottom,
			left));
};
var author$project$Helpers$ElmStyleShotcuts$pde = F4(
	function (top, right, bottom, left) {
		return mdgriffith$elm_ui$Element$paddingEach(
			{aq: bottom, at: left, az: right, aB: top});
	});
var author$project$Helpers$ElmStyleShotcuts$pdl = function (number) {
	return mdgriffith$elm_ui$Element$paddingEach(
		_Utils_update(
			author$project$Helpers$ElmStyleShotcuts$edges,
			{at: number}));
};
var author$project$Helpers$ElmStyleShotcuts$pdr = function (number) {
	return mdgriffith$elm_ui$Element$paddingEach(
		_Utils_update(
			author$project$Helpers$ElmStyleShotcuts$edges,
			{az: number}));
};
var author$project$Helpers$ElmStyleShotcuts$pdt = function (number) {
	return mdgriffith$elm_ui$Element$paddingEach(
		_Utils_update(
			author$project$Helpers$ElmStyleShotcuts$edges,
			{aB: number}));
};
var mdgriffith$elm_ui$Internal$Flag$spacing = mdgriffith$elm_ui$Internal$Flag$flag(3);
var mdgriffith$elm_ui$Internal$Model$SpacingStyle = F3(
	function (a, b, c) {
		return {$: 5, a: a, b: b, c: c};
	});
var mdgriffith$elm_ui$Internal$Model$spacingName = F2(
	function (x, y) {
		return 'spacing-' + (elm$core$String$fromInt(x) + ('-' + elm$core$String$fromInt(y)));
	});
var mdgriffith$elm_ui$Element$spacingXY = F2(
	function (x, y) {
		return A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$spacing,
			A3(
				mdgriffith$elm_ui$Internal$Model$SpacingStyle,
				A2(mdgriffith$elm_ui$Internal$Model$spacingName, x, y),
				x,
				y));
	});
var author$project$Helpers$ElmStyleShotcuts$spx = function (number) {
	return A2(mdgriffith$elm_ui$Element$spacingXY, number, 0);
};
var author$project$Helpers$ElmStyleShotcuts$spy = function (number) {
	return A2(mdgriffith$elm_ui$Element$spacingXY, 0, number);
};
var author$project$Helpers$ElmStyleShotcuts$wfmax = function (maxValue) {
	return mdgriffith$elm_ui$Element$width(
		A2(mdgriffith$elm_ui$Element$maximum, maxValue, mdgriffith$elm_ui$Element$fill));
};
var mdgriffith$elm_ui$Element$fillPortion = mdgriffith$elm_ui$Internal$Model$Fill;
var author$project$Helpers$ElmStyleShotcuts$wfp = function (number) {
	return mdgriffith$elm_ui$Element$width(
		mdgriffith$elm_ui$Element$fillPortion(number));
};
var author$project$Helpers$ElmUI$one = 0;
var author$project$Msg$ClearAllFilters = {$: 12};
var author$project$Msg$ClearSearchTextAndContinue = {$: 13};
var author$project$Msg$CloseUserWarningsDialog = function (a) {
	return {$: 9, a: a};
};
var author$project$Msg$CollapseAllClicked = function (a) {
	return {$: 6, a: a};
};
var author$project$Msg$HandleKeyboardEvent = {$: 4};
var author$project$Msg$OperateSortDialog = function (a) {
	return {$: 8, a: a};
};
var author$project$Msg$SearchPressed = {$: 3};
var author$project$Msg$SearchString = function (a) {
	return {$: 2, a: a};
};
var author$project$Helpers$ElmStyleShotcuts$pdb = function (number) {
	return mdgriffith$elm_ui$Element$paddingEach(
		_Utils_update(
			author$project$Helpers$ElmStyleShotcuts$edges,
			{aq: number}));
};
var mdgriffith$elm_ui$Internal$Flag$borderRound = mdgriffith$elm_ui$Internal$Flag$flag(17);
var mdgriffith$elm_ui$Element$Border$rounded = function (radius) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$borderRound,
		A3(
			mdgriffith$elm_ui$Internal$Model$Single,
			'br-' + elm$core$String$fromInt(radius),
			'border-radius',
			elm$core$String$fromInt(radius) + 'px'));
};
var author$project$Helpers$ElmStyleShotcuts$br = function (number) {
	return mdgriffith$elm_ui$Element$Border$rounded(number);
};
var elm$html$Html$Attributes$alt = elm$html$Html$Attributes$stringProperty('alt');
var elm$html$Html$Attributes$src = function (url) {
	return A2(
		elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var mdgriffith$elm_ui$Element$image = F2(
	function (attrs, _n0) {
		var src = _n0.aP;
		var description = _n0.aI;
		var imageAttributes = A2(
			elm$core$List$filter,
			function (a) {
				switch (a.$) {
					case 7:
						return true;
					case 8:
						return true;
					default:
						return false;
				}
			},
			attrs);
		return A4(
			mdgriffith$elm_ui$Internal$Model$element,
			mdgriffith$elm_ui$Internal$Model$asEl,
			mdgriffith$elm_ui$Internal$Model$div,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Internal$Model$htmlClass(mdgriffith$elm_ui$Internal$Style$classes.d6),
				attrs),
			mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[
						A4(
						mdgriffith$elm_ui$Internal$Model$element,
						mdgriffith$elm_ui$Internal$Model$asEl,
						mdgriffith$elm_ui$Internal$Model$NodeName('img'),
						_Utils_ap(
							_List_fromArray(
								[
									mdgriffith$elm_ui$Internal$Model$Attr(
									elm$html$Html$Attributes$src(src)),
									mdgriffith$elm_ui$Internal$Model$Attr(
									elm$html$Html$Attributes$alt(description))
								]),
							imageAttributes),
						mdgriffith$elm_ui$Internal$Model$Unkeyed(_List_Nil))
					])));
	});
var author$project$Helpers$ElmUI$buildChkBoxImage = function (userAction) {
	return userAction ? A2(
		mdgriffith$elm_ui$Element$image,
		_List_fromArray(
			[
				author$project$Helpers$ElmStyleShotcuts$hpx(16)
			]),
		{aI: 'Logo', aP: 'checked.png'}) : A2(
		mdgriffith$elm_ui$Element$el,
		_List_fromArray(
			[
				author$project$Helpers$ElmStyleShotcuts$hpx(16),
				author$project$Helpers$ElmStyleShotcuts$wpx(16),
				author$project$Helpers$ElmStyleShotcuts$bw(2),
				A3(author$project$Helpers$ElmStyleShotcuts$brc, 101, 101, 101),
				author$project$Helpers$ElmStyleShotcuts$br(3)
			]),
		mdgriffith$elm_ui$Element$none);
};
var author$project$Helpers$ElmUI$buildCollapseAllImage = function (userAction) {
	return userAction ? A2(
		mdgriffith$elm_ui$Element$image,
		_List_fromArray(
			[
				author$project$Helpers$ElmStyleShotcuts$hpx(14),
				author$project$Helpers$ElmStyleShotcuts$bw(author$project$Helpers$ElmUI$one)
			]),
		{aI: 'Logo', aP: 'collapse.png'}) : A2(
		mdgriffith$elm_ui$Element$image,
		_List_fromArray(
			[
				author$project$Helpers$ElmStyleShotcuts$hpx(14),
				author$project$Helpers$ElmStyleShotcuts$bw(author$project$Helpers$ElmUI$one)
			]),
		{aI: 'Logo', aP: 'expand.png'});
};
var author$project$Helpers$ElmStyleShotcuts$hfmax = function (maxValue) {
	return mdgriffith$elm_ui$Element$height(
		A2(mdgriffith$elm_ui$Element$maximum, maxValue, mdgriffith$elm_ui$Element$fill));
};
var mdgriffith$elm_ui$Internal$Flag$overflow = mdgriffith$elm_ui$Internal$Flag$flag(20);
var mdgriffith$elm_ui$Element$scrollbarY = A2(mdgriffith$elm_ui$Internal$Model$Class, mdgriffith$elm_ui$Internal$Flag$overflow, mdgriffith$elm_ui$Internal$Style$classes.eT);
var author$project$Helpers$ElmUI$expandCollapseAll = function (state) {
	return state ? _List_fromArray(
		[
			author$project$Helpers$ElmStyleShotcuts$hf,
			author$project$Helpers$ElmStyleShotcuts$pd(10)
		]) : _List_fromArray(
		[
			author$project$Helpers$ElmStyleShotcuts$hfmax(0),
			author$project$Helpers$ElmStyleShotcuts$pd(0),
			mdgriffith$elm_ui$Element$scrollbarY
		]);
};
var author$project$Model$SearchFilterState = F3(
	function (index, searchFilterCustomType, userAction) {
		return {bv: index, eV: searchFilterCustomType, fK: userAction};
	});
var author$project$Msg$CollapseClicked = F2(
	function (a, b) {
		return {$: 5, a: a, b: b};
	});
var author$project$Msg$FilterCheckBoxClicked = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(x);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var mdgriffith$elm_ui$Internal$Model$AsColumn = 1;
var mdgriffith$elm_ui$Internal$Model$asColumn = 1;
var mdgriffith$elm_ui$Element$column = F2(
	function (attrs, children) {
		return A4(
			mdgriffith$elm_ui$Internal$Model$element,
			mdgriffith$elm_ui$Internal$Model$asColumn,
			mdgriffith$elm_ui$Internal$Model$div,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Internal$Model$htmlClass(mdgriffith$elm_ui$Internal$Style$classes.dD + (' ' + mdgriffith$elm_ui$Internal$Style$classes.aG)),
				A2(
					elm$core$List$cons,
					mdgriffith$elm_ui$Element$height(mdgriffith$elm_ui$Element$shrink),
					A2(
						elm$core$List$cons,
						mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$shrink),
						attrs))),
			mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var elm$html$Html$Attributes$attribute = elm$virtual_dom$VirtualDom$attribute;
var mdgriffith$elm_ui$Internal$Model$Left = 0;
var mdgriffith$elm_ui$Element$alignLeft = mdgriffith$elm_ui$Internal$Model$AlignX(0);
var mdgriffith$elm_ui$Element$spacing = function (x) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$spacing,
		A3(
			mdgriffith$elm_ui$Internal$Model$SpacingStyle,
			A2(mdgriffith$elm_ui$Internal$Model$spacingName, x, x),
			x,
			x));
};
var mdgriffith$elm_ui$Element$Input$applyLabel = F3(
	function (attrs, label, input) {
		if (label.$ === 1) {
			var labelText = label.a;
			return A4(
				mdgriffith$elm_ui$Internal$Model$element,
				mdgriffith$elm_ui$Internal$Model$asColumn,
				mdgriffith$elm_ui$Internal$Model$NodeName('label'),
				attrs,
				mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[input])));
		} else {
			var position = label.a;
			var labelAttrs = label.b;
			var labelChild = label.c;
			var labelElement = A4(
				mdgriffith$elm_ui$Internal$Model$element,
				mdgriffith$elm_ui$Internal$Model$asEl,
				mdgriffith$elm_ui$Internal$Model$div,
				labelAttrs,
				mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[labelChild])));
			switch (position) {
				case 2:
					return A4(
						mdgriffith$elm_ui$Internal$Model$element,
						mdgriffith$elm_ui$Internal$Model$asColumn,
						mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						attrs,
						mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[labelElement, input])));
				case 3:
					return A4(
						mdgriffith$elm_ui$Internal$Model$element,
						mdgriffith$elm_ui$Internal$Model$asColumn,
						mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						attrs,
						mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[input, labelElement])));
				case 0:
					return A4(
						mdgriffith$elm_ui$Internal$Model$element,
						mdgriffith$elm_ui$Internal$Model$asRow,
						mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						attrs,
						mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[input, labelElement])));
				default:
					return A4(
						mdgriffith$elm_ui$Internal$Model$element,
						mdgriffith$elm_ui$Internal$Model$asRow,
						mdgriffith$elm_ui$Internal$Model$NodeName('label'),
						attrs,
						mdgriffith$elm_ui$Internal$Model$Unkeyed(
							_List_fromArray(
								[labelElement, input])));
			}
		}
	});
var mdgriffith$elm_ui$Internal$Model$Label = function (a) {
	return {$: 5, a: a};
};
var mdgriffith$elm_ui$Element$Input$hiddenLabelAttribute = function (label) {
	if (label.$ === 1) {
		var textLabel = label.a;
		return mdgriffith$elm_ui$Internal$Model$Describe(
			mdgriffith$elm_ui$Internal$Model$Label(textLabel));
	} else {
		return mdgriffith$elm_ui$Internal$Model$NoAttribute;
	}
};
var mdgriffith$elm_ui$Element$Input$isHiddenLabel = function (label) {
	if (label.$ === 1) {
		return true;
	} else {
		return false;
	}
};
var mdgriffith$elm_ui$Element$Input$onKeyLookup = function (lookup) {
	var decode = function (code) {
		var _n0 = lookup(code);
		if (_n0.$ === 1) {
			return elm$json$Json$Decode$fail('No key matched');
		} else {
			var msg = _n0.a;
			return elm$json$Json$Decode$succeed(msg);
		}
	};
	var isKey = A2(
		elm$json$Json$Decode$andThen,
		decode,
		A2(elm$json$Json$Decode$field, 'key', elm$json$Json$Decode$string));
	return mdgriffith$elm_ui$Internal$Model$Attr(
		A2(elm$html$Html$Events$on, 'keyup', isKey));
};
var mdgriffith$elm_ui$Element$Input$space = ' ';
var mdgriffith$elm_ui$Element$Input$tabindex = A2(elm$core$Basics$composeL, mdgriffith$elm_ui$Internal$Model$Attr, elm$html$Html$Attributes$tabindex);
var mdgriffith$elm_ui$Internal$Model$LivePolite = {$: 6};
var mdgriffith$elm_ui$Element$Region$announce = mdgriffith$elm_ui$Internal$Model$Describe(mdgriffith$elm_ui$Internal$Model$LivePolite);
var mdgriffith$elm_ui$Element$Input$checkbox = F2(
	function (attrs, _n0) {
		var label = _n0.ef;
		var icon = _n0.b9;
		var checked = _n0.bS;
		var onChange = _n0.cm;
		var attributes = _Utils_ap(
			_List_fromArray(
				[
					mdgriffith$elm_ui$Element$Input$isHiddenLabel(label) ? mdgriffith$elm_ui$Internal$Model$NoAttribute : mdgriffith$elm_ui$Element$spacing(6),
					mdgriffith$elm_ui$Internal$Model$Attr(
					elm$html$Html$Events$onClick(
						onChange(!checked))),
					mdgriffith$elm_ui$Element$Region$announce,
					mdgriffith$elm_ui$Element$Input$onKeyLookup(
					function (code) {
						return _Utils_eq(code, mdgriffith$elm_ui$Element$Input$enter) ? elm$core$Maybe$Just(
							onChange(!checked)) : (_Utils_eq(code, mdgriffith$elm_ui$Element$Input$space) ? elm$core$Maybe$Just(
							onChange(!checked)) : elm$core$Maybe$Nothing);
					}),
					mdgriffith$elm_ui$Element$Input$tabindex(0),
					mdgriffith$elm_ui$Element$pointer,
					mdgriffith$elm_ui$Element$alignLeft,
					mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$fill)
				]),
			attrs);
		return A3(
			mdgriffith$elm_ui$Element$Input$applyLabel,
			attributes,
			label,
			A4(
				mdgriffith$elm_ui$Internal$Model$element,
				mdgriffith$elm_ui$Internal$Model$asEl,
				mdgriffith$elm_ui$Internal$Model$div,
				_List_fromArray(
					[
						mdgriffith$elm_ui$Internal$Model$Attr(
						A2(elm$html$Html$Attributes$attribute, 'role', 'checkbox')),
						mdgriffith$elm_ui$Internal$Model$Attr(
						A2(
							elm$html$Html$Attributes$attribute,
							'aria-checked',
							checked ? 'true' : 'false')),
						mdgriffith$elm_ui$Element$Input$hiddenLabelAttribute(label),
						mdgriffith$elm_ui$Element$centerY,
						mdgriffith$elm_ui$Element$height(mdgriffith$elm_ui$Element$fill),
						mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$shrink)
					]),
				mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[
							icon(checked)
						]))));
	});
var mdgriffith$elm_ui$Element$Input$Label = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var mdgriffith$elm_ui$Element$Input$OnRight = 0;
var mdgriffith$elm_ui$Element$Input$labelRight = mdgriffith$elm_ui$Element$Input$Label(0);
var author$project$SearchFilterViews$SearchFilter$buildSearchFilterValuesGroup = F2(
	function (_n0, searchFilters) {
		var searchFilterDisplayText = _n0.eW;
		var searchFilterStates = _n0.eY;
		var showLabelRed = function (isAnyFilterChecked) {
			return isAnyFilterChecked ? author$project$Helpers$Colors$mhcRed : author$project$Helpers$Colors$greyFont(0);
		}(
			A2(
				elm$core$List$any,
				function (sf) {
					return sf.fK;
				},
				searchFilters));
		var searchFilterState = function (possbileFirstItem) {
			if (!possbileFirstItem.$) {
				var val = possbileFirstItem.a;
				return val;
			} else {
				return A3(author$project$Model$SearchFilterState, -1, 13, false);
			}
		}(
			elm$core$List$head(
				elm$core$Array$toList(searchFilterStates)));
		var _n1 = _Utils_Tuple2(searchFilterDisplayText, author$project$Msg$FilterCheckBoxClicked);
		var filterLabel = _n1.a;
		var msg = _n1.b;
		var buildCheckbox = function (searchFilter) {
			var chkBoxStyle = searchFilter.fK ? _List_fromArray(
				[author$project$Helpers$Colors$mhcRed, author$project$Helpers$ElmStyleShotcuts$fb]) : _List_fromArray(
				[
					author$project$Helpers$Colors$greyFont(0)
				]);
			return (searchFilter.cG > 0) ? A2(
				mdgriffith$elm_ui$Element$row,
				_List_fromArray(
					[
						author$project$Helpers$ElmStyleShotcuts$wf,
						mdgriffith$elm_ui$Element$Font$size(14),
						author$project$Helpers$ElmStyleShotcuts$pdl(25)
					]),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Element$Input$checkbox,
						_List_fromArray(
							[
								author$project$Helpers$ElmStyleShotcuts$bwb(1),
								author$project$Helpers$ElmStyleShotcuts$wf,
								author$project$Helpers$ElmStyleShotcuts$pdb(5),
								author$project$Helpers$Colors$greyBorder(175)
							]),
						{
							bS: searchFilter.fK,
							b9: author$project$Helpers$ElmUI$buildChkBoxImage,
							ef: A2(
								mdgriffith$elm_ui$Element$Input$labelRight,
								_Utils_ap(
									_List_fromArray(
										[mdgriffith$elm_ui$Element$centerY]),
									chkBoxStyle),
								A2(
									mdgriffith$elm_ui$Element$el,
									_List_Nil,
									author$project$Helpers$ElmUI$textValue(
										searchFilter.al + (' (' + (elm$core$String$fromInt(searchFilter.cG) + ')'))))),
							cm: msg(searchFilter)
						})
					])) : mdgriffith$elm_ui$Element$none;
		};
		return (elm$core$List$length(searchFilters) > 0) ? A2(
			mdgriffith$elm_ui$Element$row,
			_List_fromArray(
				[
					author$project$Helpers$ElmStyleShotcuts$wf,
					author$project$Helpers$ElmStyleShotcuts$pdt(5)
				]),
			_List_fromArray(
				[
					A2(
					mdgriffith$elm_ui$Element$column,
					_List_fromArray(
						[author$project$Helpers$ElmStyleShotcuts$wf]),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Element$Input$checkbox,
							_List_Nil,
							{
								bS: searchFilterState.fK,
								b9: author$project$Helpers$ElmUI$buildCollapseAllImage,
								ef: A2(
									mdgriffith$elm_ui$Element$Input$labelRight,
									_List_fromArray(
										[
											author$project$Helpers$ElmStyleShotcuts$fs(14),
											showLabelRed
										]),
									author$project$Helpers$ElmUI$textValue(filterLabel)),
								cm: author$project$Msg$CollapseClicked(searchFilterState)
							}),
							A2(
							mdgriffith$elm_ui$Element$column,
							_Utils_ap(
								_List_fromArray(
									[
										author$project$Helpers$ElmStyleShotcuts$spy(8),
										author$project$Helpers$ElmStyleShotcuts$wf
									]),
								author$project$Helpers$ElmUI$expandCollapseAll(searchFilterState.fK)),
							A2(elm$core$List$map, buildCheckbox, searchFilters))
						]))
				])) : mdgriffith$elm_ui$Element$none;
	});
var mdgriffith$elm_ui$Internal$Flag$fontAlignment = mdgriffith$elm_ui$Internal$Flag$flag(12);
var mdgriffith$elm_ui$Element$Font$center = A2(mdgriffith$elm_ui$Internal$Model$Class, mdgriffith$elm_ui$Internal$Flag$fontAlignment, mdgriffith$elm_ui$Internal$Style$classes.fi);
var author$project$Helpers$ElmStyleShotcuts$fac = mdgriffith$elm_ui$Element$Font$center;
var mdgriffith$elm_ui$Element$Font$light = A2(mdgriffith$elm_ui$Internal$Model$Class, mdgriffith$elm_ui$Internal$Flag$fontWeight, mdgriffith$elm_ui$Internal$Style$classes.fo);
var author$project$Helpers$ElmStyleShotcuts$fl = mdgriffith$elm_ui$Element$Font$light;
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var mdgriffith$elm_ui$Internal$Model$Padding = F5(
	function (a, b, c, d, e) {
		return {$: 0, a: a, b: b, c: c, d: d, e: e};
	});
var mdgriffith$elm_ui$Internal$Model$Spaced = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var mdgriffith$elm_ui$Internal$Model$extractSpacingAndPadding = function (attrs) {
	return A3(
		elm$core$List$foldr,
		F2(
			function (attr, _n0) {
				var pad = _n0.a;
				var spacing = _n0.b;
				return _Utils_Tuple2(
					function () {
						if (!pad.$) {
							var x = pad.a;
							return pad;
						} else {
							if ((attr.$ === 4) && (attr.b.$ === 7)) {
								var _n3 = attr.b;
								var name = _n3.a;
								var t = _n3.b;
								var r = _n3.c;
								var b = _n3.d;
								var l = _n3.e;
								return elm$core$Maybe$Just(
									A5(mdgriffith$elm_ui$Internal$Model$Padding, name, t, r, b, l));
							} else {
								return elm$core$Maybe$Nothing;
							}
						}
					}(),
					function () {
						if (!spacing.$) {
							var x = spacing.a;
							return spacing;
						} else {
							if ((attr.$ === 4) && (attr.b.$ === 5)) {
								var _n6 = attr.b;
								var name = _n6.a;
								var x = _n6.b;
								var y = _n6.c;
								return elm$core$Maybe$Just(
									A3(mdgriffith$elm_ui$Internal$Model$Spaced, name, x, y));
							} else {
								return elm$core$Maybe$Nothing;
							}
						}
					}());
			}),
		_Utils_Tuple2(elm$core$Maybe$Nothing, elm$core$Maybe$Nothing),
		attrs);
};
var mdgriffith$elm_ui$Element$wrappedRow = F2(
	function (attrs, children) {
		var _n0 = mdgriffith$elm_ui$Internal$Model$extractSpacingAndPadding(attrs);
		var padded = _n0.a;
		var spaced = _n0.b;
		if (spaced.$ === 1) {
			return A4(
				mdgriffith$elm_ui$Internal$Model$element,
				mdgriffith$elm_ui$Internal$Model$asRow,
				mdgriffith$elm_ui$Internal$Model$div,
				A2(
					elm$core$List$cons,
					mdgriffith$elm_ui$Internal$Model$htmlClass(mdgriffith$elm_ui$Internal$Style$classes.aG + (' ' + (mdgriffith$elm_ui$Internal$Style$classes.T + (' ' + mdgriffith$elm_ui$Internal$Style$classes.bK)))),
					A2(
						elm$core$List$cons,
						mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$shrink),
						A2(
							elm$core$List$cons,
							mdgriffith$elm_ui$Element$height(mdgriffith$elm_ui$Element$shrink),
							attrs))),
				mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
		} else {
			var _n2 = spaced.a;
			var spaceName = _n2.a;
			var x = _n2.b;
			var y = _n2.c;
			var newPadding = function () {
				if (!padded.$) {
					var _n5 = padded.a;
					var name = _n5.a;
					var t = _n5.b;
					var r = _n5.c;
					var b = _n5.d;
					var l = _n5.e;
					return ((_Utils_cmp(r, (x / 2) | 0) > -1) && (_Utils_cmp(b, (y / 2) | 0) > -1)) ? elm$core$Maybe$Just(
						mdgriffith$elm_ui$Element$paddingEach(
							{aq: b - ((y / 2) | 0), at: l - ((x / 2) | 0), az: r - ((x / 2) | 0), aB: t - ((y / 2) | 0)})) : elm$core$Maybe$Nothing;
				} else {
					return elm$core$Maybe$Nothing;
				}
			}();
			if (!newPadding.$) {
				var pad = newPadding.a;
				return A4(
					mdgriffith$elm_ui$Internal$Model$element,
					mdgriffith$elm_ui$Internal$Model$asRow,
					mdgriffith$elm_ui$Internal$Model$div,
					A2(
						elm$core$List$cons,
						mdgriffith$elm_ui$Internal$Model$htmlClass(mdgriffith$elm_ui$Internal$Style$classes.aG + (' ' + (mdgriffith$elm_ui$Internal$Style$classes.T + (' ' + mdgriffith$elm_ui$Internal$Style$classes.bK)))),
						A2(
							elm$core$List$cons,
							mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$shrink),
							A2(
								elm$core$List$cons,
								mdgriffith$elm_ui$Element$height(mdgriffith$elm_ui$Element$shrink),
								_Utils_ap(
									attrs,
									_List_fromArray(
										[pad]))))),
					mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
			} else {
				var halfY = -(y / 2);
				var halfX = -(x / 2);
				return A4(
					mdgriffith$elm_ui$Internal$Model$element,
					mdgriffith$elm_ui$Internal$Model$asEl,
					mdgriffith$elm_ui$Internal$Model$div,
					attrs,
					mdgriffith$elm_ui$Internal$Model$Unkeyed(
						_List_fromArray(
							[
								A4(
								mdgriffith$elm_ui$Internal$Model$element,
								mdgriffith$elm_ui$Internal$Model$asRow,
								mdgriffith$elm_ui$Internal$Model$div,
								A2(
									elm$core$List$cons,
									mdgriffith$elm_ui$Internal$Model$htmlClass(mdgriffith$elm_ui$Internal$Style$classes.aG + (' ' + (mdgriffith$elm_ui$Internal$Style$classes.T + (' ' + mdgriffith$elm_ui$Internal$Style$classes.bK)))),
									A2(
										elm$core$List$cons,
										mdgriffith$elm_ui$Internal$Model$Attr(
											A2(
												elm$html$Html$Attributes$style,
												'margin',
												elm$core$String$fromFloat(halfY) + ('px' + (' ' + (elm$core$String$fromFloat(halfX) + 'px'))))),
										A2(
											elm$core$List$cons,
											mdgriffith$elm_ui$Internal$Model$Attr(
												A2(
													elm$html$Html$Attributes$style,
													'width',
													'calc(100% + ' + (elm$core$String$fromInt(x) + 'px)'))),
											A2(
												elm$core$List$cons,
												mdgriffith$elm_ui$Internal$Model$Attr(
													A2(
														elm$html$Html$Attributes$style,
														'height',
														'calc(100% + ' + (elm$core$String$fromInt(y) + 'px)'))),
												A2(
													elm$core$List$cons,
													A2(
														mdgriffith$elm_ui$Internal$Model$StyleClass,
														mdgriffith$elm_ui$Internal$Flag$spacing,
														A3(mdgriffith$elm_ui$Internal$Model$SpacingStyle, spaceName, x, y)),
													_List_Nil))))),
								mdgriffith$elm_ui$Internal$Model$Unkeyed(children))
							])));
			}
		}
	});
var author$project$TruckViews$SearchFilterBullet$searchFilterBulletView = function (filterList) {
	return A2(
		mdgriffith$elm_ui$Element$wrappedRow,
		_List_fromArray(
			[author$project$Helpers$ElmStyleShotcuts$wf]),
		A2(
			elm$core$List$map,
			function (sf) {
				return sf.fK ? A2(
					mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[
							A4(author$project$Helpers$ElmStyleShotcuts$pde, 3, 3, 3, 0)
						]),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Element$column,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Element$el,
									_List_fromArray(
										[
											author$project$Helpers$ElmStyleShotcuts$fs(15),
											author$project$Helpers$Colors$greyBorder(97),
											author$project$Helpers$Colors$mhcRed,
											author$project$Helpers$Colors$greyBg(231),
											author$project$Helpers$ElmStyleShotcuts$pd(6),
											author$project$Helpers$ElmStyleShotcuts$fl,
											author$project$Helpers$ElmStyleShotcuts$hf
										]),
									author$project$Helpers$ElmUI$textValue(sf.al))
								])),
							A2(
							mdgriffith$elm_ui$Element$column,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Element$Input$button,
									_List_fromArray(
										[
											author$project$Helpers$Colors$greyBorder(97),
											author$project$Helpers$Colors$greyBg(231),
											author$project$Helpers$ElmStyleShotcuts$pd(6)
										]),
									{
										ef: A2(
											mdgriffith$elm_ui$Element$el,
											_List_fromArray(
												[
													author$project$Helpers$ElmStyleShotcuts$wpx(15),
													author$project$Helpers$ElmStyleShotcuts$hpx(15),
													author$project$Helpers$ElmStyleShotcuts$fs(14),
													author$project$Helpers$ElmStyleShotcuts$fac,
													author$project$Helpers$Colors$greyFont(300),
													author$project$Helpers$Colors$greyBg(150)
												]),
											author$project$Helpers$ElmUI$textValue('x')),
										eu: elm$core$Maybe$Just(
											A2(author$project$Msg$FilterCheckBoxClicked, sf, false))
									})
								]))
						])) : mdgriffith$elm_ui$Element$none;
			},
			filterList));
};
var author$project$Model$MakeZtoA = 5;
var author$project$Model$MileageHighToLow = 3;
var author$project$Model$MileageLowToHigh = 2;
var author$project$Model$PriceHighToLow = 1;
var author$project$Model$PriceLowToHigh = 0;
var author$project$Model$YearNewToOld = 6;
var author$project$Model$YearOldToNew = 7;
var author$project$BusinessFunctions$TruckFunctions$sortByItemslist = _List_fromArray(
	[
		{Y: 'Price', Z: 0, _: 'Price - Low to High', aa: 'ASC'},
		{Y: 'Price', Z: 1, _: 'Price - High to Low', aa: 'DESC'},
		{Y: 'Mileage', Z: 2, _: 'Mileage - Low to High', aa: 'ASC'},
		{Y: 'Mileage', Z: 3, _: 'Mileage - High to Low', aa: 'DESC'},
		{Y: 'Make', Z: 4, _: 'Make A to Z', aa: 'ASC'},
		{Y: 'Make', Z: 5, _: 'Make Z to A', aa: 'DESC'},
		{Y: 'Year', Z: 7, _: 'Year - Old to New', aa: 'ASC'},
		{Y: 'Year', Z: 6, _: 'Year - New to Old', aa: 'DESC'}
	]);
var mdgriffith$elm_ui$Internal$Flag$borderStyle = mdgriffith$elm_ui$Internal$Flag$flag(11);
var mdgriffith$elm_ui$Element$Border$dotted = A2(mdgriffith$elm_ui$Internal$Model$Class, mdgriffith$elm_ui$Internal$Flag$borderStyle, mdgriffith$elm_ui$Internal$Style$classes.$7);
var author$project$Helpers$ElmStyleShotcuts$bdot = mdgriffith$elm_ui$Element$Border$dotted;
var mdgriffith$elm_ui$Element$Font$alignLeft = A2(mdgriffith$elm_ui$Internal$Model$Class, mdgriffith$elm_ui$Internal$Flag$fontAlignment, mdgriffith$elm_ui$Internal$Style$classes.fn);
var author$project$Helpers$ElmStyleShotcuts$fal = mdgriffith$elm_ui$Element$Font$alignLeft;
var author$project$Msg$SortTrucks = function (a) {
	return {$: 10, a: a};
};
var author$project$TruckViews$SortDialog$buildSortOption = F2(
	function (sortMetaDataItem, currentSortByField) {
		var _n0 = _Utils_eq(sortMetaDataItem.Z, currentSortByField) ? _Utils_Tuple2(
			_List_fromArray(
				[
					author$project$Helpers$Colors$greyBg(175)
				]),
			_List_Nil) : _Utils_Tuple2(
			_List_fromArray(
				[
					author$project$Helpers$Colors$greyBg(245)
				]),
			_List_fromArray(
				[
					author$project$Helpers$Colors$greyBg(225)
				]));
		var rowStyle = _n0.a;
		var mouseOverStyle = _n0.b;
		return A2(
			mdgriffith$elm_ui$Element$row,
			_Utils_ap(
				_List_fromArray(
					[
						author$project$Helpers$ElmStyleShotcuts$wf,
						mdgriffith$elm_ui$Element$mouseOver(mouseOverStyle)
					]),
				rowStyle),
			_List_fromArray(
				[
					A2(
					mdgriffith$elm_ui$Element$Input$button,
					_List_fromArray(
						[author$project$Helpers$ElmStyleShotcuts$hf, author$project$Helpers$ElmStyleShotcuts$fal, author$project$Helpers$ElmStyleShotcuts$wf]),
					{
						ef: A2(
							mdgriffith$elm_ui$Element$el,
							_List_fromArray(
								[
									author$project$Helpers$ElmStyleShotcuts$pd(3)
								]),
							author$project$Helpers$ElmUI$textValue(sortMetaDataItem._)),
						eu: elm$core$Maybe$Just(
							author$project$Msg$SortTrucks(sortMetaDataItem))
					})
				]));
	});
var elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						elm$core$List$cons,
						sep,
						A2(elm$core$List$cons, x, rest));
				});
			var spersed = A3(elm$core$List$foldr, step, _List_Nil, tl);
			return A2(elm$core$List$cons, hd, spersed);
		}
	});
var author$project$TruckViews$SortDialog$showSortOptionsDialog = F2(
	function (show, currentSortByChoice) {
		return show ? A2(
			mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					author$project$Helpers$Colors$greyBg(245),
					author$project$Helpers$ElmStyleShotcuts$pd(15),
					author$project$Helpers$ElmStyleShotcuts$br(5),
					author$project$Helpers$ElmStyleShotcuts$bw(2),
					author$project$Helpers$ElmStyleShotcuts$wpx(300)
				]),
			_List_fromArray(
				[
					A2(
					mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[
							author$project$Helpers$ElmStyleShotcuts$ear,
							author$project$Helpers$ElmStyleShotcuts$bw(1),
							author$project$Helpers$Colors$greyBg(228)
						]),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Element$Input$button,
							_List_Nil,
							{
								ef: author$project$Helpers$ElmUI$textValue(' x '),
								eu: elm$core$Maybe$Just(
									author$project$Msg$OperateSortDialog(false))
							})
						])),
					A2(
					mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[
							author$project$Helpers$ElmStyleShotcuts$wf,
							author$project$Helpers$ElmStyleShotcuts$pdt(10)
						]),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Element$column,
							_List_fromArray(
								[
									author$project$Helpers$ElmStyleShotcuts$spy(3),
									author$project$Helpers$ElmStyleShotcuts$wf
								]),
							A2(
								elm$core$List$intersperse,
								A2(
									mdgriffith$elm_ui$Element$el,
									_List_fromArray(
										[
											author$project$Helpers$ElmStyleShotcuts$bwb(1),
											author$project$Helpers$ElmStyleShotcuts$bdot,
											author$project$Helpers$ElmStyleShotcuts$wf
										]),
									author$project$Helpers$ElmUI$textValue('')),
								A2(
									elm$core$List$map,
									function (sortMetaDataItem) {
										return A2(author$project$TruckViews$SortDialog$buildSortOption, sortMetaDataItem, currentSortByChoice.Z);
									},
									author$project$BusinessFunctions$TruckFunctions$sortByItemslist)))
						]))
				])) : mdgriffith$elm_ui$Element$none;
	});
var author$project$BusinessFunctions$TruckFunctions$buildTruckIdNumber = function (truck) {
	return (truck.cO > 0) ? _Utils_Tuple2(
		'Stock#: ',
		'i0' + elm$core$String$fromInt(truck.cO)) : ((truck.bO > 0) ? _Utils_Tuple2(
		'Appraisal#: ',
		'A' + elm$core$String$fromInt(truck.bO)) : _Utils_Tuple2('PO#: ', 'P' + truck.eF));
};
var mdgriffith$elm_ui$Element$Font$extraBold = A2(mdgriffith$elm_ui$Internal$Model$Class, mdgriffith$elm_ui$Internal$Flag$fontWeight, mdgriffith$elm_ui$Internal$Style$classes.fj);
var author$project$Helpers$ElmStyleShotcuts$feb = mdgriffith$elm_ui$Element$Font$extraBold;
var elm$core$Basics$modBy = _Basics_modBy;
var ggb$numeral_elm$Languages$English$englishOrdinal = function (number) {
	var number1 = elm$core$Basics$floor(number);
	var b = A2(elm$core$Basics$modBy, 10, number1);
	return (elm$core$Basics$floor(
		A2(elm$core$Basics$modBy, 100, number1) / 10) === 1) ? 'th' : ((b === 1) ? 'st' : ((b === 2) ? 'nd' : ((b === 3) ? 'rd' : 'th')));
};
var ggb$numeral_elm$Languages$English$lang = {
	cZ: {dh: 'b', em: 'm', fv: 'k', fC: 't'},
	dF: {d: '$'},
	dJ: {dI: '.', fw: ','},
	eA: ggb$numeral_elm$Languages$English$englishOrdinal
};
var ggb$numeral_elm$Numeral$empty = F3(
	function (lang, format_val, value) {
		return {
			Q: '',
			aW: '',
			a1: '',
			dI: '',
			l: format_val,
			ah: lang,
			a5: 0,
			a9: '',
			av: false,
			aw: false,
			ba: false,
			eA: '',
			bc: _Utils_Tuple2('', ''),
			bd: '',
			be: '',
			aO: false,
			bh: elm$core$String$fromFloat(value),
			P: value,
			s: ''
		};
	});
var elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var elm$core$Basics$pow = _Basics_pow;
var elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {bv: index, ek: match, er: number, ff: submatches};
	});
var elm$regex$Regex$replace = _Regex_replaceAtMost(_Regex_infinity);
var elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var elm$regex$Regex$fromString = function (string) {
	return A2(
		elm$regex$Regex$fromStringWith,
		{bQ: false, cj: false},
		string);
};
var elm$regex$Regex$never = _Regex_never;
var ggb$numeral_elm$Numeral$regex = function (str) {
	return A2(
		elm$core$Maybe$withDefault,
		elm$regex$Regex$never,
		elm$regex$Regex$fromString(str));
};
var ggb$numeral_elm$Numeral$emptyReplace = function (str) {
	return A2(
		elm$regex$Regex$replace,
		ggb$numeral_elm$Numeral$regex(str),
		function (_n0) {
			return '';
		});
};
var ggb$numeral_elm$Numeral$checkAbbreviation = function (numeral) {
	var _n0 = numeral;
	var language = _n0.ah;
	var format_value = _n0.l;
	var value = _n0.P;
	var abbrB = A2(elm$core$String$contains, 'aB', format_value);
	var abbrK = A2(elm$core$String$contains, 'aK', format_value);
	var abbrM = A2(elm$core$String$contains, 'aM', format_value);
	var abbrT = A2(elm$core$String$contains, 'aT', format_value);
	var abbrForce = !(abbrK || (abbrM || (abbrB || abbrT)));
	var absValue = elm$core$Basics$abs(value);
	var _n1 = A2(elm$core$String$contains, ' a', format_value) ? _Utils_Tuple2(
		' ',
		A2(ggb$numeral_elm$Numeral$emptyReplace, ' a', format_value)) : _Utils_Tuple2(
		'',
		A2(ggb$numeral_elm$Numeral$emptyReplace, 'a', format_value));
	var abbr = _n1.a;
	var format1 = _n1.b;
	return (!A2(elm$core$String$contains, 'a', format_value)) ? numeral : ((((_Utils_cmp(
		absValue,
		A2(elm$core$Basics$pow, 10, 12)) > -1) && abbrForce) || abbrT) ? _Utils_update(
		numeral,
		{
			Q: _Utils_ap(abbr, language.cZ.fC),
			l: format1,
			P: value / A2(elm$core$Basics$pow, 10, 12)
		}) : ((((_Utils_cmp(
		absValue,
		A2(elm$core$Basics$pow, 10, 12)) < 0) && ((_Utils_cmp(
		absValue,
		A2(elm$core$Basics$pow, 10, 9)) > -1) && abbrForce)) || abbrB) ? _Utils_update(
		numeral,
		{
			Q: _Utils_ap(abbr, language.cZ.dh),
			l: format1,
			P: value / A2(elm$core$Basics$pow, 10, 9)
		}) : ((((_Utils_cmp(
		absValue,
		A2(elm$core$Basics$pow, 10, 9)) < 0) && ((_Utils_cmp(
		absValue,
		A2(elm$core$Basics$pow, 10, 6)) > -1) && abbrForce)) || abbrM) ? _Utils_update(
		numeral,
		{
			Q: _Utils_ap(abbr, language.cZ.em),
			l: format1,
			P: value / A2(elm$core$Basics$pow, 10, 6)
		}) : ((((_Utils_cmp(
		absValue,
		A2(elm$core$Basics$pow, 10, 6)) < 0) && ((_Utils_cmp(
		absValue,
		A2(elm$core$Basics$pow, 10, 3)) > -1) && abbrForce)) || abbrK) ? _Utils_update(
		numeral,
		{
			Q: _Utils_ap(abbr, language.cZ.fv),
			l: format1,
			P: value / A2(elm$core$Basics$pow, 10, 3)
		}) : _Utils_update(
		numeral,
		{Q: abbr, l: format1})))));
};
var elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = elm$core$Array$bitMask & (index >>> shift);
			var _n0 = A2(elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (!_n0.$) {
				var subTree = _n0.a;
				var $temp$shift = shift - elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _n0.a;
				return A2(elm$core$Elm$JsArray$unsafeGet, elm$core$Array$bitMask & index, values);
			}
		}
	});
var elm$core$Array$get = F2(
	function (index, _n0) {
		var len = _n0.a;
		var startShift = _n0.b;
		var tree = _n0.c;
		var tail = _n0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			elm$core$Array$tailIndex(len)) > -1) ? elm$core$Maybe$Just(
			A2(elm$core$Elm$JsArray$unsafeGet, elm$core$Array$bitMask & index, tail)) : elm$core$Maybe$Just(
			A3(elm$core$Array$getHelp, startShift, index, tree)));
	});
var ggb$numeral_elm$Numeral$suffixes = elm$core$Array$fromList(
	_List_fromArray(
		['B', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB']));
var ggb$numeral_elm$Numeral$checkByte = function (numeral) {
	var _n0 = numeral;
	var format_value = _n0.l;
	var value = _n0.P;
	var suffixIndex1 = function (power) {
		suffixIndex1:
		while (true) {
			var minValue = A2(elm$core$Basics$pow, 1024, power);
			var maxValue = A2(elm$core$Basics$pow, 1024, power + 1);
			if ((_Utils_cmp(value, minValue) > -1) && (_Utils_cmp(value, maxValue) < 0)) {
				return (minValue > 0) ? _Utils_Tuple2(power, value / minValue) : _Utils_Tuple2(power, value);
			} else {
				if (power < 10) {
					var $temp$power = power + 1;
					power = $temp$power;
					continue suffixIndex1;
				} else {
					return _Utils_Tuple2(-1, value);
				}
			}
		}
	};
	var _n1 = suffixIndex1(0);
	var suffixIndex = _n1.a;
	var value1 = _n1.b;
	var suffix = A2(
		elm$core$Maybe$withDefault,
		'',
		A2(
			elm$core$Array$get,
			elm$core$Basics$round(suffixIndex),
			ggb$numeral_elm$Numeral$suffixes));
	var _n2 = A2(elm$core$String$contains, ' b', format_value) ? _Utils_Tuple2(
		' ',
		A2(ggb$numeral_elm$Numeral$emptyReplace, ' b', format_value)) : _Utils_Tuple2(
		'',
		A2(ggb$numeral_elm$Numeral$emptyReplace, 'b', format_value));
	var bytes = _n2.a;
	var format1 = _n2.b;
	return A2(elm$core$String$contains, 'b', format_value) ? _Utils_update(
		numeral,
		{
			aW: _Utils_ap(bytes, suffix),
			l: format1,
			P: value1
		}) : numeral;
};
var elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			elm$core$String$join,
			after,
			A2(elm$core$String$split, before, string));
	});
var elm$regex$Regex$find = _Regex_findAtMost(_Regex_infinity);
var ggb$numeral_elm$Numeral$checkForCustomSuffix = function (numeral) {
	var hasSuffix = elm$core$List$head(
		A2(
			elm$regex$Regex$find,
			ggb$numeral_elm$Numeral$regex('\\[\\D+\\]$'),
			numeral.l));
	if (hasSuffix.$ === 1) {
		return numeral;
	} else {
		var match = hasSuffix.a.ek;
		return _Utils_update(
			numeral,
			{
				a1: A3(
					elm$regex$Regex$replace,
					ggb$numeral_elm$Numeral$regex('\\[|\\]'),
					function (_n1) {
						return '';
					},
					match),
				l: A3(elm$core$String$replace, match, '', numeral.l)
			});
	}
};
var ggb$numeral_elm$Numeral$checkIfNegative = function (numeral) {
	return A2(elm$core$String$contains, '-', numeral.s) ? _Utils_update(
		numeral,
		{
			av: true,
			s: A3(
				elm$core$String$slice,
				1,
				elm$core$String$length(numeral.s),
				numeral.s)
		}) : numeral;
};
var ggb$numeral_elm$Numeral$checkOptionalDec = function (numeral) {
	return A2(elm$core$String$contains, '[.]', numeral.l) ? _Utils_update(
		numeral,
		{
			l: A3(
				elm$regex$Regex$replace,
				ggb$numeral_elm$Numeral$regex('\\[\\.\\]'),
				function (_n0) {
					return '.';
				},
				numeral.l),
			ba: true
		}) : numeral;
};
var ggb$numeral_elm$Numeral$checkOrdinal = function (numeral) {
	var _n0 = numeral;
	var language = _n0.ah;
	var format_value = _n0.l;
	var value = _n0.P;
	var _n1 = A2(elm$core$String$contains, ' o', format_value) ? _Utils_Tuple2(
		' ',
		A2(ggb$numeral_elm$Numeral$emptyReplace, ' o', format_value)) : _Utils_Tuple2(
		'',
		A2(ggb$numeral_elm$Numeral$emptyReplace, 'o', format_value));
	var ord = _n1.a;
	var format1 = _n1.b;
	return A2(elm$core$String$contains, 'o', format_value) ? _Utils_update(
		numeral,
		{
			l: format1,
			eA: _Utils_ap(
				ord,
				numeral.ah.eA(value))
		}) : numeral;
};
var ggb$numeral_elm$Numeral$checkParensAndSign = function (numeral) {
	return A2(elm$core$String$contains, '(', numeral.l) ? _Utils_update(
		numeral,
		{
			l: A3(elm$core$String$slice, 1, -1, numeral.l),
			aw: true,
			aO: false
		}) : (A2(elm$core$String$contains, '+', numeral.l) ? _Utils_update(
		numeral,
		{
			l: A2(ggb$numeral_elm$Numeral$emptyReplace, '\\+', numeral.l),
			aw: false,
			aO: true
		}) : numeral);
};
var ggb$numeral_elm$Numeral$addThousandsDelimiter = F2(
	function (lang, word) {
		return A3(
			elm$regex$Regex$replace,
			ggb$numeral_elm$Numeral$regex('(\\d)(?=(\\d{3})+(?!\\d))'),
			function (_n0) {
				var match = _n0.ek;
				return _Utils_ap(match, lang.dJ.fw);
			},
			word);
	});
var ggb$numeral_elm$Numeral$checkThousandsDelimiter = function (numeral) {
	return A2(elm$core$String$contains, ',', numeral.l) ? _Utils_update(
		numeral,
		{
			s: A2(ggb$numeral_elm$Numeral$addThousandsDelimiter, numeral.ah, numeral.s)
		}) : numeral;
};
var ggb$numeral_elm$Numeral$createFinalString = function (_n0) {
	var parens = _n0.bc;
	var minus = _n0.a9;
	var plus = _n0.bd;
	var word = _n0.s;
	var decimal = _n0.dI;
	var ordinal = _n0.eA;
	var abbreviation = _n0.Q;
	var bytes = _n0.aW;
	var customSuffix = _n0.a1;
	return A2(
		elm$core$String$join,
		'',
		_List_fromArray(
			[parens.a, minus, plus, word, decimal, ordinal, abbreviation, bytes, customSuffix, parens.b]));
};
var ggb$numeral_elm$Numeral$indexOf = F2(
	function (part, word) {
		return A2(
			elm$core$Maybe$withDefault,
			-1,
			elm$core$List$head(
				A2(elm$core$String$indexes, part, word)));
	});
var ggb$numeral_elm$Numeral$createFinalWord = function (numeral) {
	return (!A2(ggb$numeral_elm$Numeral$indexOf, '.', numeral.l)) ? _Utils_update(
		numeral,
		{s: ''}) : numeral;
};
var ggb$numeral_elm$Numeral$createParens = function (numeral) {
	return (numeral.aw && numeral.av) ? _Utils_update(
		numeral,
		{
			bc: _Utils_Tuple2('(', ')')
		}) : numeral;
};
var elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var ggb$numeral_elm$Numeral$getPrecision = function (numeral) {
	var splitted = A2(elm$core$String$split, '.', numeral.l);
	var precision = A2(
		elm$core$Maybe$withDefault,
		'',
		elm$core$List$head(
			A2(elm$core$List$drop, 1, splitted)));
	var leadingZeros = elm$core$String$length(
		A2(
			elm$core$Maybe$withDefault,
			'',
			elm$core$List$head(splitted)));
	var leadingZeros_ = A2(elm$core$String$contains, ',', numeral.l) ? 0 : leadingZeros;
	return _Utils_update(
		numeral,
		{a5: leadingZeros_, be: precision});
};
var ggb$numeral_elm$Numeral$hasMinus = function (numeral) {
	return ((!numeral.aw) && numeral.av) ? _Utils_update(
		numeral,
		{a9: '-'}) : numeral;
};
var ggb$numeral_elm$Numeral$hasPlus = function (numeral) {
	return ((!numeral.av) && numeral.aO) ? _Utils_update(
		numeral,
		{bd: '+'}) : numeral;
};
var ggb$numeral_elm$Numeral$processDecimal = function (numeral) {
	var d = numeral.dI;
	var result = A2(
		elm$core$Maybe$withDefault,
		1,
		elm$core$String$toInt(
			A3(
				elm$core$String$slice,
				1,
				elm$core$String$length(d),
				d)));
	return (numeral.ba && (!result)) ? _Utils_update(
		numeral,
		{dI: ''}) : _Utils_update(
		numeral,
		{dI: d});
};
var elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3(elm$core$String$repeatHelp, n, chunk, '');
	});
var ggb$numeral_elm$Numeral$processLeadingZeros = function (numeral) {
	var w = elm$core$String$length(
		A2(
			elm$core$Maybe$withDefault,
			'',
			elm$core$List$head(
				A2(elm$core$String$split, '.', numeral.s))));
	var n = numeral.a5 - w;
	var prefix = A2(elm$core$String$repeat, n, '0');
	return _Utils_update(
		numeral,
		{
			s: _Utils_ap(prefix, numeral.s)
		});
};
var elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2(elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var elm$core$List$takeTailRec = F2(
	function (n, list) {
		return elm$core$List$reverse(
			A3(elm$core$List$takeReverse, n, list, _List_Nil));
	});
var elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _n0 = _Utils_Tuple2(n, list);
			_n0$1:
			while (true) {
				_n0$5:
				while (true) {
					if (!_n0.b.b) {
						return list;
					} else {
						if (_n0.b.b.b) {
							switch (_n0.a) {
								case 1:
									break _n0$1;
								case 2:
									var _n2 = _n0.b;
									var x = _n2.a;
									var _n3 = _n2.b;
									var y = _n3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_n0.b.b.b.b) {
										var _n4 = _n0.b;
										var x = _n4.a;
										var _n5 = _n4.b;
										var y = _n5.a;
										var _n6 = _n5.b;
										var z = _n6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _n0$5;
									}
								default:
									if (_n0.b.b.b.b && _n0.b.b.b.b.b) {
										var _n7 = _n0.b;
										var x = _n7.a;
										var _n8 = _n7.b;
										var y = _n8.a;
										var _n9 = _n8.b;
										var z = _n9.a;
										var _n10 = _n9.b;
										var w = _n10.a;
										var tl = _n10.b;
										return (ctr > 1000) ? A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A2(elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A3(elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _n0$5;
									}
							}
						} else {
							if (_n0.a === 1) {
								break _n0$1;
							} else {
								break _n0$5;
							}
						}
					}
				}
				return list;
			}
			var _n1 = _n0.b;
			var x = _n1.a;
			return _List_fromArray(
				[x]);
		}
	});
var elm$core$List$take = F2(
	function (n, list) {
		return A3(elm$core$List$takeFast, 0, n, list);
	});
var ggb$numeral_elm$Numeral$flip = F3(
	function (f, x, y) {
		return A2(f, y, x);
	});
var elm$core$String$cons = _String_cons;
var elm$core$String$fromChar = function (_char) {
	return A2(elm$core$String$cons, _char, '');
};
var elm$core$String$padRight = F3(
	function (n, _char, string) {
		return _Utils_ap(
			string,
			A2(
				elm$core$String$repeat,
				n - elm$core$String$length(string),
				elm$core$String$fromChar(_char)));
	});
var ggb$numeral_elm$Numeral$toFixed = F2(
	function (precision, value) {
		var power = A2(elm$core$Basics$pow, 10, precision);
		var pad = function (num) {
			_n0$2:
			while (true) {
				if (num.b) {
					if (num.b.b) {
						if (!num.b.b.b) {
							var x = num.a;
							var _n1 = num.b;
							var y = _n1.a;
							return _List_fromArray(
								[
									x,
									A3(elm$core$String$padRight, precision, '0', y)
								]);
						} else {
							break _n0$2;
						}
					} else {
						var val = num.a;
						return _List_fromArray(
							[
								val,
								A3(elm$core$String$padRight, precision, '0', '')
							]);
					}
				} else {
					break _n0$2;
				}
			}
			var val = num;
			return val;
		};
		return A2(
			elm$core$String$join,
			'.',
			pad(
				A2(
					elm$core$String$split,
					'.',
					elm$core$String$fromFloat(
						elm$core$Basics$round((value * power) + 1.0e-2) / power))));
	});
var ggb$numeral_elm$Numeral$toFixedWithOptional = F2(
	function (prs, value) {
		_n0$2:
		while (true) {
			if (prs.b) {
				if (prs.b.b) {
					if (!prs.b.b.b) {
						var x = prs.a;
						var _n1 = prs.b;
						var y = _n1.a;
						return A2(
							ggb$numeral_elm$Numeral$emptyReplace,
							'0{1,' + (elm$core$String$fromInt(y) + '}$'),
							A2(ggb$numeral_elm$Numeral$toFixed, x + y, value));
					} else {
						break _n0$2;
					}
				} else {
					var x = prs.a;
					return A2(ggb$numeral_elm$Numeral$toFixed, x, value);
				}
			} else {
				break _n0$2;
			}
		}
		return elm$core$String$fromFloat(value);
	});
var ggb$numeral_elm$Numeral$processPrecision = function (numeral) {
	var _n0 = numeral;
	var language = _n0.ah;
	var format_value = _n0.l;
	var value = _n0.P;
	var precision = _n0.be;
	var fst = A2(elm$core$String$contains, '[', precision) ? A2(
		ggb$numeral_elm$Numeral$flip(ggb$numeral_elm$Numeral$toFixedWithOptional),
		value,
		A2(
			elm$core$List$take,
			2,
			A2(
				elm$core$List$map,
				elm$core$String$length,
				A2(
					elm$core$String$split,
					'[',
					A2(ggb$numeral_elm$Numeral$emptyReplace, ']', precision))))) : A2(
		ggb$numeral_elm$Numeral$toFixed,
		elm$core$String$length(precision),
		value);
	var snd = function () {
		var _n1 = A2(elm$core$String$split, '.', fst);
		if ((_n1.b && _n1.b.b) && (!_n1.b.b.b)) {
			var x = _n1.a;
			var _n2 = _n1.b;
			var y = _n2.a;
			return (elm$core$String$length(y) > 0) ? _Utils_ap(language.dJ.dI, y) : '';
		} else {
			return '';
		}
	}();
	var w = A2(
		elm$core$Maybe$withDefault,
		'',
		elm$core$List$head(
			A2(elm$core$String$split, '.', fst)));
	return (precision === '') ? _Utils_update(
		numeral,
		{dI: '', s: w}) : _Utils_update(
		numeral,
		{dI: snd, s: w});
};
var ggb$numeral_elm$Numeral$processWord = function (numeral) {
	var w = A2(
		elm$core$Maybe$withDefault,
		'',
		elm$core$List$head(
			A2(elm$core$String$split, '.', numeral.bh)));
	return _Utils_update(
		numeral,
		{s: w});
};
var ggb$numeral_elm$Numeral$updateStringValue = function (numeral) {
	return _Utils_update(
		numeral,
		{
			bh: elm$core$String$fromFloat(numeral.P)
		});
};
var ggb$numeral_elm$Numeral$formatNumber = function (numeral) {
	return ggb$numeral_elm$Numeral$createFinalString(
		ggb$numeral_elm$Numeral$hasPlus(
			ggb$numeral_elm$Numeral$hasMinus(
				ggb$numeral_elm$Numeral$createParens(
					ggb$numeral_elm$Numeral$createFinalWord(
						ggb$numeral_elm$Numeral$checkIfNegative(
							ggb$numeral_elm$Numeral$checkThousandsDelimiter(
								ggb$numeral_elm$Numeral$processDecimal(
									ggb$numeral_elm$Numeral$processLeadingZeros(
										ggb$numeral_elm$Numeral$processPrecision(
											ggb$numeral_elm$Numeral$getPrecision(
												ggb$numeral_elm$Numeral$processWord(
													ggb$numeral_elm$Numeral$updateStringValue(
														ggb$numeral_elm$Numeral$checkOptionalDec(
															ggb$numeral_elm$Numeral$checkOrdinal(
																ggb$numeral_elm$Numeral$checkByte(
																	ggb$numeral_elm$Numeral$checkAbbreviation(
																		ggb$numeral_elm$Numeral$checkForCustomSuffix(
																			ggb$numeral_elm$Numeral$checkParensAndSign(numeral)))))))))))))))))));
};
var ggb$numeral_elm$Numeral$formatWithoutCurrency = function (lformat) {
	return A2(elm$core$String$contains, ' $', lformat) ? _Utils_Tuple2(
		' ',
		A2(ggb$numeral_elm$Numeral$emptyReplace, ' \\$', lformat)) : (A2(elm$core$String$contains, '$ ', lformat) ? _Utils_Tuple2(
		' ',
		A2(ggb$numeral_elm$Numeral$emptyReplace, '\\$ ', lformat)) : _Utils_Tuple2(
		'',
		A2(ggb$numeral_elm$Numeral$emptyReplace, '\\$', lformat)));
};
var ggb$numeral_elm$Numeral$formatCurrency = F4(
	function (lang, lformat, value, strValue) {
		var symbolIndex = A2(ggb$numeral_elm$Numeral$indexOf, '$', lformat);
		var openParenIndex = A2(ggb$numeral_elm$Numeral$indexOf, '(', lformat);
		var minusSignIndex = A2(ggb$numeral_elm$Numeral$indexOf, '-', lformat);
		var currencySymbol = lang.dF.d;
		var _n0 = ggb$numeral_elm$Numeral$formatWithoutCurrency(lformat);
		var space = _n0.a;
		var format1 = _n0.b;
		var formatted = ggb$numeral_elm$Numeral$formatNumber(
			A3(ggb$numeral_elm$Numeral$empty, lang, format1, value));
		return (symbolIndex <= 1) ? ((A2(elm$core$String$contains, '(', formatted) || A2(elm$core$String$contains, '-', formatted)) ? (((_Utils_cmp(symbolIndex, openParenIndex) < 0) || (_Utils_cmp(symbolIndex, minusSignIndex) < 0)) ? A2(
			elm$core$String$join,
			'',
			_List_fromArray(
				[
					currencySymbol,
					space,
					A2(elm$core$String$contains, '-', formatted) ? '-' : '',
					A2(elm$core$String$contains, '(', formatted) ? '(' : '',
					A3(
					elm$core$String$slice,
					1,
					elm$core$String$length(formatted),
					formatted)
				])) : A2(
			elm$core$String$join,
			'',
			_List_fromArray(
				[
					A2(elm$core$String$contains, '-', formatted) ? '-' : '',
					A2(elm$core$String$contains, '(', formatted) ? '(' : '',
					currencySymbol,
					space,
					A3(
					elm$core$String$slice,
					1,
					elm$core$String$length(formatted),
					formatted)
				]))) : _Utils_ap(
			currencySymbol,
			_Utils_ap(space, formatted))) : (A2(elm$core$String$contains, ')', formatted) ? A2(
			elm$core$String$join,
			'',
			_List_fromArray(
				[
					A3(
					elm$core$String$slice,
					0,
					elm$core$String$length(formatted) - 1,
					formatted),
					space,
					currencySymbol,
					')'
				])) : _Utils_ap(
			formatted,
			_Utils_ap(space, currencySymbol)));
	});
var ggb$numeral_elm$Numeral$formatWithoutPercent = function (lformat) {
	return A2(elm$core$String$contains, ' %', lformat) ? _Utils_Tuple2(
		' ',
		A2(ggb$numeral_elm$Numeral$emptyReplace, ' %', lformat)) : _Utils_Tuple2(
		'',
		A2(ggb$numeral_elm$Numeral$emptyReplace, '%', lformat));
};
var ggb$numeral_elm$Numeral$formatPercentage = F4(
	function (lang, lformat, value, strValue) {
		var value1 = value * 100;
		var _n0 = ggb$numeral_elm$Numeral$formatWithoutPercent(lformat);
		var space = _n0.a;
		var format1 = _n0.b;
		var formatted = ggb$numeral_elm$Numeral$formatNumber(
			A3(ggb$numeral_elm$Numeral$empty, lang, format1, value1));
		return A2(elm$core$String$contains, ')', formatted) ? A2(
			elm$core$String$join,
			'',
			_List_fromArray(
				[
					A3(
					elm$core$String$slice,
					0,
					elm$core$String$length(formatted) - 1,
					formatted),
					space,
					'%',
					')'
				])) : (formatted + (space + '%'));
	});
var ggb$numeral_elm$Numeral$formatTime = F4(
	function (lang, lformat, value, strValue) {
		var hours = elm$core$Basics$floor((value / 60) / 60);
		var minutes = elm$core$Basics$floor((value - ((hours * 60) * 60)) / 60);
		var seconds = elm$core$Basics$round((value - ((hours * 60) * 60)) - (minutes * 60));
		var hasOneDigit = function (val) {
			return (elm$core$String$length(val) < 2) ? ('0' + val) : val;
		};
		return A2(
			elm$core$String$join,
			':',
			_List_fromArray(
				[
					elm$core$String$fromFloat(hours),
					hasOneDigit(
					elm$core$String$fromFloat(minutes)),
					hasOneDigit(
					elm$core$String$fromInt(seconds))
				]));
	});
var ggb$numeral_elm$Numeral$formatWithLanguage = F3(
	function (lang, lformat, value) {
		return A2(elm$core$String$contains, '$', lformat) ? A4(
			ggb$numeral_elm$Numeral$formatCurrency,
			lang,
			lformat,
			value,
			elm$core$String$fromFloat(value)) : (A2(elm$core$String$contains, '%', lformat) ? A4(
			ggb$numeral_elm$Numeral$formatPercentage,
			lang,
			lformat,
			value,
			elm$core$String$fromFloat(value)) : (A2(elm$core$String$contains, ':', lformat) ? A4(
			ggb$numeral_elm$Numeral$formatTime,
			lang,
			lformat,
			value,
			elm$core$String$fromFloat(value)) : ggb$numeral_elm$Numeral$formatNumber(
			A3(ggb$numeral_elm$Numeral$empty, lang, lformat, value))));
	});
var ggb$numeral_elm$Numeral$format = ggb$numeral_elm$Numeral$formatWithLanguage(ggb$numeral_elm$Languages$English$lang);
var author$project$TruckViews$Truck$buildPriceValue = function (price) {
	return (!price) ? 'Call for pricing' : A2(ggb$numeral_elm$Numeral$format, '$0,0', price);
};
var mdgriffith$elm_ui$Internal$Model$Paragraph = {$: 9};
var mdgriffith$elm_ui$Element$paragraph = F2(
	function (attrs, children) {
		return A4(
			mdgriffith$elm_ui$Internal$Model$element,
			mdgriffith$elm_ui$Internal$Model$asParagraph,
			mdgriffith$elm_ui$Internal$Model$div,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Internal$Model$Describe(mdgriffith$elm_ui$Internal$Model$Paragraph),
				A2(
					elm$core$List$cons,
					mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$fill),
					A2(
						elm$core$List$cons,
						mdgriffith$elm_ui$Element$spacing(5),
						attrs))),
			mdgriffith$elm_ui$Internal$Model$Unkeyed(children));
	});
var author$project$TruckViews$Truck$dataFieldView = F2(
	function (fieldName, fieldValue) {
		return A2(
			mdgriffith$elm_ui$Element$row,
			_List_fromArray(
				[
					author$project$Helpers$ElmStyleShotcuts$bw(0),
					author$project$Helpers$ElmStyleShotcuts$wf
				]),
			_List_fromArray(
				[
					A2(
					mdgriffith$elm_ui$Element$paragraph,
					_List_fromArray(
						[
							author$project$Helpers$ElmStyleShotcuts$fal,
							author$project$Helpers$ElmStyleShotcuts$spy(1),
							author$project$Helpers$ElmStyleShotcuts$fs(18),
							author$project$Helpers$Colors$greyFont(105)
						]),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Element$el,
							_List_fromArray(
								[author$project$Helpers$ElmStyleShotcuts$fb]),
							author$project$Helpers$ElmUI$textValue(fieldName)),
							A2(
							mdgriffith$elm_ui$Element$el,
							_List_fromArray(
								[
									author$project$Helpers$ElmStyleShotcuts$pdl(5),
									author$project$Helpers$ElmStyleShotcuts$fs(16),
									author$project$Helpers$Colors$greyFont(97)
								]),
							author$project$Helpers$ElmUI$textValue(fieldValue))
						]))
				]));
	});
var elm$core$String$trim = _String_trim;
var elm$html$Html$Attributes$target = elm$html$Html$Attributes$stringProperty('target');
var mdgriffith$elm_ui$Internal$Flag$transparency = mdgriffith$elm_ui$Internal$Flag$flag(0);
var mdgriffith$elm_ui$Internal$Model$Transparency = F2(
	function (a, b) {
		return {$: 12, a: a, b: b};
	});
var mdgriffith$elm_ui$Element$alpha = function (o) {
	var transparency = function (x) {
		return 1 - x;
	}(
		A2(
			elm$core$Basics$min,
			1.0,
			A2(elm$core$Basics$max, 0.0, o)));
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$transparency,
		A2(
			mdgriffith$elm_ui$Internal$Model$Transparency,
			'transparency-' + mdgriffith$elm_ui$Internal$Model$floatClass(transparency),
			transparency));
};
var mdgriffith$elm_ui$Element$htmlAttribute = mdgriffith$elm_ui$Internal$Model$Attr;
var mdgriffith$elm_ui$Internal$Model$InFront = 4;
var mdgriffith$elm_ui$Element$inFront = function (element) {
	return A2(mdgriffith$elm_ui$Internal$Model$Nearby, 4, element);
};
var elm$html$Html$Attributes$href = function (url) {
	return A2(
		elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var elm$html$Html$Attributes$rel = _VirtualDom_attribute('rel');
var mdgriffith$elm_ui$Element$link = F2(
	function (attrs, _n0) {
		var url = _n0.fJ;
		var label = _n0.ef;
		return A4(
			mdgriffith$elm_ui$Internal$Model$element,
			mdgriffith$elm_ui$Internal$Model$asEl,
			mdgriffith$elm_ui$Internal$Model$NodeName('a'),
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Internal$Model$Attr(
					elm$html$Html$Attributes$href(url)),
				A2(
					elm$core$List$cons,
					mdgriffith$elm_ui$Internal$Model$Attr(
						elm$html$Html$Attributes$rel('noopener noreferrer')),
					A2(
						elm$core$List$cons,
						mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$shrink),
						A2(
							elm$core$List$cons,
							mdgriffith$elm_ui$Element$height(mdgriffith$elm_ui$Element$shrink),
							A2(
								elm$core$List$cons,
								mdgriffith$elm_ui$Internal$Model$htmlClass(mdgriffith$elm_ui$Internal$Style$classes.a$ + (' ' + mdgriffith$elm_ui$Internal$Style$classes.T)),
								attrs))))),
			mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[label])));
	});
var author$project$TruckViews$Truck$truckView = F2(
	function (index, truck) {
		var isTruckSold = (truck.eQ === 'I') || (truck.eQ === 'S');
		var formatImageLink = A3(
			elm$core$String$replace,
			'&thn=1',
			'&thn=2',
			A3(
				elm$core$String$replace,
				'&w=16',
				'&w=225',
				A3(
					elm$core$String$replace,
					'&h=16',
					'',
					function (isLinkEmpty) {
						return isLinkEmpty ? 'photoscomingsoon.png' : truck.eG;
					}(
						elm$core$String$isEmpty(truck.eG)))));
		return A2(
			mdgriffith$elm_ui$Element$row,
			_List_fromArray(
				[
					author$project$Helpers$ElmStyleShotcuts$wf,
					author$project$Helpers$ElmStyleShotcuts$pd(5),
					author$project$Helpers$Colors$greyBg(240)
				]),
			_List_fromArray(
				[
					A2(
					mdgriffith$elm_ui$Element$column,
					_List_fromArray(
						[author$project$Helpers$ElmStyleShotcuts$hf]),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Element$image,
							_List_fromArray(
								[
									author$project$Helpers$ElmStyleShotcuts$hpx(110),
									author$project$Helpers$ElmStyleShotcuts$wpx(125),
									author$project$Helpers$Colors$greyBg(250),
									mdgriffith$elm_ui$Element$inFront(
									isTruckSold ? A2(
										mdgriffith$elm_ui$Element$el,
										_List_fromArray(
											[
												mdgriffith$elm_ui$Element$alignBottom,
												author$project$Helpers$Colors$greyFont(250),
												A3(author$project$Helpers$ElmStyleShotcuts$bc, 234, 67, 82),
												author$project$Helpers$ElmStyleShotcuts$wf,
												mdgriffith$elm_ui$Element$alpha(0.55),
												author$project$Helpers$ElmStyleShotcuts$feb,
												author$project$Helpers$ElmStyleShotcuts$fac
											]),
										author$project$Helpers$ElmUI$textValue('SOLD')) : mdgriffith$elm_ui$Element$none)
								]),
							{aI: 'Logo', aP: formatImageLink})
						])),
					A2(
					mdgriffith$elm_ui$Element$column,
					_List_fromArray(
						[
							author$project$Helpers$ElmStyleShotcuts$wf,
							author$project$Helpers$ElmStyleShotcuts$hf,
							A4(author$project$Helpers$ElmStyleShotcuts$pde, 3, 3, 3, 15),
							author$project$Helpers$ElmStyleShotcuts$spy(10)
						]),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Element$row,
							_List_fromArray(
								[author$project$Helpers$ElmStyleShotcuts$wf]),
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Element$link,
									_List_fromArray(
										[
											mdgriffith$elm_ui$Element$htmlAttribute(
											elm$html$Html$Attributes$target('_blank'))
										]),
									{
										ef: A2(
											mdgriffith$elm_ui$Element$el,
											_List_fromArray(
												[
													author$project$Helpers$ElmStyleShotcuts$fs(24),
													author$project$Helpers$ElmStyleShotcuts$fb,
													author$project$Helpers$Colors$mhcRed
												]),
											author$project$Helpers$ElmUI$textValue(truck.fy)),
										fJ: A3(
											elm$url$Url$Builder$crossOrigin,
											'https://www.mhc.com/trucks/used',
											_List_fromArray(
												[
													truck.fP,
													truck.ej,
													truck.ep,
													function (tup) {
													return tup.b;
												}(
													author$project$BusinessFunctions$TruckFunctions$buildTruckIdNumber(truck))
												]),
											_List_Nil)
									})
								])),
							A2(
							mdgriffith$elm_ui$Element$row,
							_List_fromArray(
								[
									author$project$Helpers$ElmStyleShotcuts$bw(0),
									author$project$Helpers$ElmStyleShotcuts$spx(15)
								]),
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Element$el,
									_List_fromArray(
										[
											author$project$Helpers$ElmStyleShotcuts$fs(24),
											author$project$Helpers$ElmStyleShotcuts$fb,
											author$project$Helpers$Colors$greyFont(68),
											author$project$Helpers$ElmStyleShotcuts$wpx(155)
										]),
									author$project$Helpers$ElmUI$textValue(
										author$project$TruckViews$Truck$buildPriceValue(truck.bA))),
									((!truck.cO) && (elm$core$String$trim(truck.fF) !== 'P')) ? A2(
									mdgriffith$elm_ui$Element$el,
									_List_fromArray(
										[
											author$project$Helpers$ElmStyleShotcuts$fs(22),
											author$project$Helpers$Colors$greyFont(167),
											author$project$Helpers$ElmStyleShotcuts$bw(0)
										]),
									author$project$Helpers$ElmUI$textValue(
										'Appraisal# - ' + elm$core$String$fromInt(truck.bO))) : mdgriffith$elm_ui$Element$none
								])),
							A2(
							mdgriffith$elm_ui$Element$row,
							_List_fromArray(
								[
									author$project$Helpers$ElmStyleShotcuts$hf,
									author$project$Helpers$ElmStyleShotcuts$wf,
									author$project$Helpers$ElmStyleShotcuts$fs(16),
									author$project$Helpers$ElmStyleShotcuts$bw(0)
								]),
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Element$column,
									_List_fromArray(
										[
											author$project$Helpers$ElmStyleShotcuts$bw(0),
											author$project$Helpers$ElmStyleShotcuts$wfmax(300),
											author$project$Helpers$ElmStyleShotcuts$hf,
											author$project$Helpers$ElmStyleShotcuts$pd(0),
											author$project$Helpers$ElmStyleShotcuts$spy(5)
										]),
									_List_fromArray(
										[
											A2(
											author$project$TruckViews$Truck$dataFieldView,
											'Location:',
											elm$core$String$isEmpty(truck.eh) ? 'N/A' : truck.eh),
											A2(
											author$project$TruckViews$Truck$dataFieldView,
											'Stock#:',
											(!truck.cO) ? 'N/A' : elm$core$String$fromInt(truck.cO)),
											A2(
											author$project$TruckViews$Truck$dataFieldView,
											'Chassis#:',
											A2(elm$core$String$dropLeft, 9, truck.dw))
										])),
									A2(
									mdgriffith$elm_ui$Element$column,
									_List_fromArray(
										[
											author$project$Helpers$ElmStyleShotcuts$bw(0),
											author$project$Helpers$ElmStyleShotcuts$wfmax(250),
											author$project$Helpers$ElmStyleShotcuts$hf,
											author$project$Helpers$ElmStyleShotcuts$pd(0),
											author$project$Helpers$ElmStyleShotcuts$spy(5)
										]),
									_List_fromArray(
										[
											A2(
											author$project$TruckViews$Truck$dataFieldView,
											'Mileage:',
											A2(ggb$numeral_elm$Numeral$format, '0,0', truck.el)),
											A2(
											author$project$TruckViews$Truck$dataFieldView,
											'Sleeper Size:',
											(!truck.e4) ? 'Non-Sleeper' : elm$core$String$fromFloat(truck.e4)),
											A2(
											author$project$TruckViews$Truck$dataFieldView,
											'Horsepower:',
											elm$core$String$fromFloat(truck.dM))
										])),
									A2(
									mdgriffith$elm_ui$Element$column,
									_List_fromArray(
										[
											author$project$Helpers$ElmStyleShotcuts$bw(0),
											author$project$Helpers$ElmStyleShotcuts$wf,
											author$project$Helpers$ElmStyleShotcuts$hf,
											author$project$Helpers$ElmStyleShotcuts$pd(0),
											author$project$Helpers$ElmStyleShotcuts$spy(5)
										]),
									_List_fromArray(
										[
											A2(author$project$TruckViews$Truck$dataFieldView, 'Engine Make:', truck.dN),
											A2(author$project$TruckViews$Truck$dataFieldView, 'Engine Model:', truck.dO),
											A2(author$project$TruckViews$Truck$dataFieldView, 'Transmission:', truck.fB)
										]))
								]))
						]))
				]));
	});
var author$project$TruckViews$Truck$trucksView = function (trucks) {
	return A2(
		mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[
				author$project$Helpers$ElmStyleShotcuts$hf,
				author$project$Helpers$ElmStyleShotcuts$wf,
				author$project$Helpers$ElmStyleShotcuts$spy(5)
			]),
		A2(elm$core$List$indexedMap, author$project$TruckViews$Truck$truckView, trucks));
};
var elm$core$Array$filter = F2(
	function (isGood, array) {
		return elm$core$Array$fromList(
			A3(
				elm$core$Array$foldr,
				F2(
					function (x, xs) {
						return isGood(x) ? A2(elm$core$List$cons, x, xs) : xs;
					}),
				_List_Nil,
				array));
	});
var elm$html$Html$Events$keyCode = A2(elm$json$Json$Decode$field, 'keyCode', elm$json$Json$Decode$int);
var elm_community$html_extra$Html$Events$Extra$onEnter = function (onEnterAction) {
	return A2(
		elm$html$Html$Events$on,
		'keyup',
		A2(
			elm$json$Json$Decode$andThen,
			function (keyCode) {
				return (keyCode === 13) ? elm$json$Json$Decode$succeed(onEnterAction) : elm$json$Json$Decode$fail(
					elm$core$String$fromInt(keyCode));
			},
			elm$html$Html$Events$keyCode));
};
var mdgriffith$elm_ui$Internal$Model$Below = 1;
var mdgriffith$elm_ui$Element$below = function (element) {
	return A2(mdgriffith$elm_ui$Internal$Model$Nearby, 1, element);
};
var mdgriffith$elm_ui$Element$clipY = A2(mdgriffith$elm_ui$Internal$Model$Class, mdgriffith$elm_ui$Internal$Flag$overflow, mdgriffith$elm_ui$Internal$Style$classes.dA);
var mdgriffith$elm_ui$Internal$Model$FocusStyleOption = function (a) {
	return {$: 1, a: a};
};
var mdgriffith$elm_ui$Element$focusStyle = mdgriffith$elm_ui$Internal$Model$FocusStyleOption;
var mdgriffith$elm_ui$Internal$Model$OnlyDynamic = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$AllowHover = 1;
var mdgriffith$elm_ui$Internal$Model$Layout = 1;
var mdgriffith$elm_ui$Internal$Model$focusDefaultStyle = {
	dd: elm$core$Maybe$Nothing,
	dm: elm$core$Maybe$Nothing,
	e$: elm$core$Maybe$Just(
		{
			di: 3,
			dB: A4(mdgriffith$elm_ui$Internal$Model$Rgba, 155 / 255, 203 / 255, 1, 1),
			es: _Utils_Tuple2(0, 0),
			e1: 3
		})
};
var mdgriffith$elm_ui$Internal$Model$optionsToRecord = function (options) {
	var combine = F2(
		function (opt, record) {
			switch (opt.$) {
				case 0:
					var hoverable = opt.a;
					var _n4 = record.d3;
					if (_n4.$ === 1) {
						return _Utils_update(
							record,
							{
								d3: elm$core$Maybe$Just(hoverable)
							});
					} else {
						return record;
					}
				case 1:
					var focusStyle = opt.a;
					var _n5 = record.dY;
					if (_n5.$ === 1) {
						return _Utils_update(
							record,
							{
								dY: elm$core$Maybe$Just(focusStyle)
							});
					} else {
						return record;
					}
				default:
					var renderMode = opt.a;
					var _n6 = record.eo;
					if (_n6.$ === 1) {
						return _Utils_update(
							record,
							{
								eo: elm$core$Maybe$Just(renderMode)
							});
					} else {
						return record;
					}
			}
		});
	var andFinally = function (record) {
		return {
			dY: function () {
				var _n0 = record.dY;
				if (_n0.$ === 1) {
					return mdgriffith$elm_ui$Internal$Model$focusDefaultStyle;
				} else {
					var focusable = _n0.a;
					return focusable;
				}
			}(),
			d3: function () {
				var _n1 = record.d3;
				if (_n1.$ === 1) {
					return 1;
				} else {
					var hoverable = _n1.a;
					return hoverable;
				}
			}(),
			eo: function () {
				var _n2 = record.eo;
				if (_n2.$ === 1) {
					return 1;
				} else {
					var actualMode = _n2.a;
					return actualMode;
				}
			}()
		};
	};
	return andFinally(
		A3(
			elm$core$List$foldr,
			combine,
			{dY: elm$core$Maybe$Nothing, d3: elm$core$Maybe$Nothing, eo: elm$core$Maybe$Nothing},
			options));
};
var mdgriffith$elm_ui$Internal$Model$toHtml = F2(
	function (mode, el) {
		switch (el.$) {
			case 0:
				var html = el.a;
				return html(mdgriffith$elm_ui$Internal$Model$asEl);
			case 1:
				var styles = el.a.fe;
				var html = el.a.d4;
				return A2(
					html,
					mode(styles),
					mdgriffith$elm_ui$Internal$Model$asEl);
			case 2:
				var text = el.a;
				return mdgriffith$elm_ui$Internal$Model$textElement(text);
			default:
				return mdgriffith$elm_ui$Internal$Model$textElement('');
		}
	});
var mdgriffith$elm_ui$Internal$Model$renderRoot = F3(
	function (optionList, attributes, child) {
		var options = mdgriffith$elm_ui$Internal$Model$optionsToRecord(optionList);
		var embedStyle = function () {
			var _n0 = options.eo;
			if (_n0 === 2) {
				return mdgriffith$elm_ui$Internal$Model$OnlyDynamic(options);
			} else {
				return mdgriffith$elm_ui$Internal$Model$StaticRootAndDynamic(options);
			}
		}();
		return A2(
			mdgriffith$elm_ui$Internal$Model$toHtml,
			embedStyle,
			A4(
				mdgriffith$elm_ui$Internal$Model$element,
				mdgriffith$elm_ui$Internal$Model$asEl,
				mdgriffith$elm_ui$Internal$Model$div,
				attributes,
				mdgriffith$elm_ui$Internal$Model$Unkeyed(
					_List_fromArray(
						[child]))));
	});
var mdgriffith$elm_ui$Internal$Flag$fontFamily = mdgriffith$elm_ui$Internal$Flag$flag(5);
var mdgriffith$elm_ui$Internal$Model$FontFamily = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var mdgriffith$elm_ui$Internal$Model$SansSerif = {$: 1};
var mdgriffith$elm_ui$Internal$Model$Typeface = function (a) {
	return {$: 3, a: a};
};
var elm$core$String$words = _String_words;
var mdgriffith$elm_ui$Internal$Model$renderFontClassName = F2(
	function (font, current) {
		return _Utils_ap(
			current,
			function () {
				switch (font.$) {
					case 0:
						return 'serif';
					case 1:
						return 'sans-serif';
					case 2:
						return 'monospace';
					case 3:
						var name = font.a;
						return A2(
							elm$core$String$join,
							'-',
							elm$core$String$words(
								elm$core$String$toLower(name)));
					case 4:
						var name = font.a;
						var url = font.b;
						return A2(
							elm$core$String$join,
							'-',
							elm$core$String$words(
								elm$core$String$toLower(name)));
					default:
						var name = font.a.eq;
						return A2(
							elm$core$String$join,
							'-',
							elm$core$String$words(
								elm$core$String$toLower(name)));
				}
			}());
	});
var mdgriffith$elm_ui$Internal$Model$rootStyle = function () {
	var families = _List_fromArray(
		[
			mdgriffith$elm_ui$Internal$Model$Typeface('Open Sans'),
			mdgriffith$elm_ui$Internal$Model$Typeface('Helvetica'),
			mdgriffith$elm_ui$Internal$Model$Typeface('Verdana'),
			mdgriffith$elm_ui$Internal$Model$SansSerif
		]);
	return _List_fromArray(
		[
			A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$bgColor,
			A3(
				mdgriffith$elm_ui$Internal$Model$Colored,
				'bg-color-' + mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4(mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0)),
				'background-color',
				A4(mdgriffith$elm_ui$Internal$Model$Rgba, 1, 1, 1, 0))),
			A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$fontColor,
			A3(
				mdgriffith$elm_ui$Internal$Model$Colored,
				'font-color-' + mdgriffith$elm_ui$Internal$Model$formatColorClass(
					A4(mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1)),
				'color',
				A4(mdgriffith$elm_ui$Internal$Model$Rgba, 0, 0, 0, 1))),
			A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$fontSize,
			mdgriffith$elm_ui$Internal$Model$FontSize(20)),
			A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$fontFamily,
			A2(
				mdgriffith$elm_ui$Internal$Model$FontFamily,
				A3(elm$core$List$foldl, mdgriffith$elm_ui$Internal$Model$renderFontClassName, 'font-', families),
				families))
		]);
}();
var mdgriffith$elm_ui$Element$layoutWith = F3(
	function (_n0, attrs, child) {
		var options = _n0.ez;
		return A3(
			mdgriffith$elm_ui$Internal$Model$renderRoot,
			options,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Internal$Model$htmlClass(
					A2(
						elm$core$String$join,
						' ',
						_List_fromArray(
							[mdgriffith$elm_ui$Internal$Style$classes.eN, mdgriffith$elm_ui$Internal$Style$classes.da, mdgriffith$elm_ui$Internal$Style$classes.e0]))),
				_Utils_ap(mdgriffith$elm_ui$Internal$Model$rootStyle, attrs)),
			child);
	});
var mdgriffith$elm_ui$Internal$Flag$shadows = mdgriffith$elm_ui$Internal$Flag$flag(19);
var mdgriffith$elm_ui$Internal$Model$boxShadowName = function (shadow) {
	return elm$core$String$concat(
		_List_fromArray(
			[
				shadow.cc ? 'box-inset' : 'box-',
				elm$core$String$fromFloat(shadow.es.a) + 'px',
				elm$core$String$fromFloat(shadow.es.b) + 'px',
				elm$core$String$fromFloat(shadow.di) + 'px',
				elm$core$String$fromFloat(shadow.e1) + 'px',
				mdgriffith$elm_ui$Internal$Model$formatColorClass(shadow.dB)
			]));
};
var mdgriffith$elm_ui$Element$Border$shadow = function (almostShade) {
	var shade = {di: almostShade.di, dB: almostShade.dB, cc: false, es: almostShade.es, e1: almostShade.e1};
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$shadows,
		A3(
			mdgriffith$elm_ui$Internal$Model$Single,
			mdgriffith$elm_ui$Internal$Model$boxShadowName(shade),
			'box-shadow',
			mdgriffith$elm_ui$Internal$Model$formatBoxShadow(shade)));
};
var mdgriffith$elm_ui$Internal$Model$ImportFont = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var mdgriffith$elm_ui$Element$Font$external = function (_n0) {
	var url = _n0.fJ;
	var name = _n0.eq;
	return A2(mdgriffith$elm_ui$Internal$Model$ImportFont, name, url);
};
var mdgriffith$elm_ui$Element$Font$family = function (families) {
	return A2(
		mdgriffith$elm_ui$Internal$Model$StyleClass,
		mdgriffith$elm_ui$Internal$Flag$fontFamily,
		A2(
			mdgriffith$elm_ui$Internal$Model$FontFamily,
			A3(elm$core$List$foldl, mdgriffith$elm_ui$Internal$Model$renderFontClassName, 'ff-', families),
			families));
};
var mdgriffith$elm_ui$Element$Font$sansSerif = mdgriffith$elm_ui$Internal$Model$SansSerif;
var mdgriffith$elm_ui$Element$Input$OnLeft = 1;
var mdgriffith$elm_ui$Element$Input$labelLeft = mdgriffith$elm_ui$Element$Input$Label(1);
var mdgriffith$elm_ui$Element$Input$Placeholder = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var mdgriffith$elm_ui$Element$Input$placeholder = mdgriffith$elm_ui$Element$Input$Placeholder;
var mdgriffith$elm_ui$Element$Input$TextInputNode = function (a) {
	return {$: 0, a: a};
};
var elm$html$Html$Attributes$type_ = elm$html$Html$Attributes$stringProperty('type');
var elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3(elm$core$List$foldr, elm$json$Json$Decode$field, decoder, fields);
	});
var elm$html$Html$Events$targetValue = A2(
	elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	elm$json$Json$Decode$string);
var elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			elm$json$Json$Decode$map,
			elm$html$Html$Events$alwaysStop,
			A2(elm$json$Json$Decode$map, tagger, elm$html$Html$Events$targetValue)));
};
var mdgriffith$elm_ui$Element$rgba = mdgriffith$elm_ui$Internal$Model$Rgba;
var mdgriffith$elm_ui$Element$Input$Padding = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var mdgriffith$elm_ui$Element$Input$autofill = A2(
	elm$core$Basics$composeL,
	mdgriffith$elm_ui$Internal$Model$Attr,
	elm$html$Html$Attributes$attribute('autocomplete'));
var mdgriffith$elm_ui$Element$rgb = F3(
	function (r, g, b) {
		return A4(mdgriffith$elm_ui$Internal$Model$Rgba, r, g, b, 1);
	});
var mdgriffith$elm_ui$Element$Input$charcoal = A3(mdgriffith$elm_ui$Element$rgb, 136 / 255, 138 / 255, 133 / 255);
var mdgriffith$elm_ui$Element$Input$darkGrey = A3(mdgriffith$elm_ui$Element$rgb, 186 / 255, 189 / 255, 182 / 255);
var mdgriffith$elm_ui$Element$paddingXY = F2(
	function (x, y) {
		return _Utils_eq(x, y) ? A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$padding,
			A5(
				mdgriffith$elm_ui$Internal$Model$PaddingStyle,
				'p-' + elm$core$String$fromInt(x),
				x,
				x,
				x,
				x)) : A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$padding,
			A5(
				mdgriffith$elm_ui$Internal$Model$PaddingStyle,
				'p-' + (elm$core$String$fromInt(x) + ('-' + elm$core$String$fromInt(y))),
				y,
				x,
				y,
				x));
	});
var mdgriffith$elm_ui$Element$Input$defaultTextPadding = A2(mdgriffith$elm_ui$Element$paddingXY, 12, 12);
var mdgriffith$elm_ui$Element$Input$white = A3(mdgriffith$elm_ui$Element$rgb, 1, 1, 1);
var mdgriffith$elm_ui$Element$Input$defaultTextBoxStyle = _List_fromArray(
	[
		mdgriffith$elm_ui$Element$Input$defaultTextPadding,
		mdgriffith$elm_ui$Element$Border$rounded(3),
		mdgriffith$elm_ui$Element$Border$color(mdgriffith$elm_ui$Element$Input$darkGrey),
		mdgriffith$elm_ui$Element$Background$color(mdgriffith$elm_ui$Element$Input$white),
		mdgriffith$elm_ui$Element$Border$width(1),
		mdgriffith$elm_ui$Element$spacing(3),
		mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$fill)
	]);
var mdgriffith$elm_ui$Element$Input$inheritablePlaceholderAttributes = function (attr) {
	_n0$3:
	while (true) {
		if (attr.$ === 4) {
			switch (attr.b.$) {
				case 7:
					var _n1 = attr.b;
					return true;
				case 6:
					var _n2 = attr.b;
					return true;
				case 10:
					return true;
				default:
					break _n0$3;
			}
		} else {
			break _n0$3;
		}
	}
	return false;
};
var elm$html$Html$Attributes$spellcheck = elm$html$Html$Attributes$boolProperty('spellcheck');
var mdgriffith$elm_ui$Element$Input$spellcheck = A2(elm$core$Basics$composeL, mdgriffith$elm_ui$Internal$Model$Attr, elm$html$Html$Attributes$spellcheck);
var elm$core$String$lines = _String_lines;
var mdgriffith$elm_ui$Internal$Flag$heightTextAreaContent = mdgriffith$elm_ui$Internal$Flag$flag(47);
var mdgriffith$elm_ui$Element$Input$textHeightContent = F4(
	function (textValue, spacing, maybePadding, maybeBorder) {
		var topBottom = function (_n0) {
			var t = _n0.a;
			var b = _n0.c;
			return t + b;
		};
		var newlineCount = function (x) {
			return (x < 1) ? 1 : x;
		}(
			elm$core$List$length(
				elm$core$String$lines(textValue)));
		var additionalSpacing = (((newlineCount - 1) * spacing) + A2(
			elm$core$Maybe$withDefault,
			0,
			A2(elm$core$Maybe$map, topBottom, maybePadding))) + A2(
			elm$core$Maybe$withDefault,
			0,
			A2(elm$core$Maybe$map, topBottom, maybeBorder));
		var heightValue = function (count) {
			return 'calc(' + (elm$core$String$fromInt(count) + ('em + ' + (elm$core$String$fromInt(additionalSpacing) + 'px) !important')));
		};
		return A2(
			mdgriffith$elm_ui$Internal$Model$StyleClass,
			mdgriffith$elm_ui$Internal$Flag$heightTextAreaContent,
			A3(
				mdgriffith$elm_ui$Internal$Model$Single,
				'textarea-height-' + elm$core$String$fromInt(newlineCount),
				'height',
				heightValue(newlineCount)));
	});
var elm$html$Html$Attributes$value = elm$html$Html$Attributes$stringProperty('value');
var mdgriffith$elm_ui$Element$Input$value = A2(elm$core$Basics$composeL, mdgriffith$elm_ui$Internal$Model$Attr, elm$html$Html$Attributes$value);
var mdgriffith$elm_ui$Internal$Model$filter = function (attrs) {
	return A3(
		elm$core$List$foldr,
		F2(
			function (x, _n0) {
				var found = _n0.a;
				var has = _n0.b;
				switch (x.$) {
					case 0:
						return _Utils_Tuple2(found, has);
					case 3:
						var key = x.a;
						return _Utils_Tuple2(
							A2(elm$core$List$cons, x, found),
							has);
					case 1:
						var attr = x.a;
						return _Utils_Tuple2(
							A2(elm$core$List$cons, x, found),
							has);
					case 4:
						var style = x.b;
						return _Utils_Tuple2(
							A2(elm$core$List$cons, x, found),
							has);
					case 7:
						var width = x.a;
						return A2(elm$core$Set$member, 'width', has) ? _Utils_Tuple2(found, has) : _Utils_Tuple2(
							A2(elm$core$List$cons, x, found),
							A2(elm$core$Set$insert, 'width', has));
					case 8:
						var height = x.a;
						return A2(elm$core$Set$member, 'height', has) ? _Utils_Tuple2(found, has) : _Utils_Tuple2(
							A2(elm$core$List$cons, x, found),
							A2(elm$core$Set$insert, 'height', has));
					case 2:
						var description = x.a;
						return A2(elm$core$Set$member, 'described', has) ? _Utils_Tuple2(found, has) : _Utils_Tuple2(
							A2(elm$core$List$cons, x, found),
							A2(elm$core$Set$insert, 'described', has));
					case 9:
						var location = x.a;
						var elem = x.b;
						return _Utils_Tuple2(
							A2(elm$core$List$cons, x, found),
							has);
					case 6:
						return A2(elm$core$Set$member, 'align-x', has) ? _Utils_Tuple2(found, has) : _Utils_Tuple2(
							A2(elm$core$List$cons, x, found),
							A2(elm$core$Set$insert, 'align-x', has));
					case 5:
						return A2(elm$core$Set$member, 'align-y', has) ? _Utils_Tuple2(found, has) : _Utils_Tuple2(
							A2(elm$core$List$cons, x, found),
							A2(elm$core$Set$insert, 'align-y', has));
					default:
						return A2(elm$core$Set$member, 'transform', has) ? _Utils_Tuple2(found, has) : _Utils_Tuple2(
							A2(elm$core$List$cons, x, found),
							A2(elm$core$Set$insert, 'transform', has));
				}
			}),
		_Utils_Tuple2(_List_Nil, elm$core$Set$empty),
		attrs).a;
};
var mdgriffith$elm_ui$Internal$Model$get = F2(
	function (attrs, isAttr) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, found) {
					return isAttr(x) ? A2(elm$core$List$cons, x, found) : found;
				}),
			_List_Nil,
			mdgriffith$elm_ui$Internal$Model$filter(attrs));
	});
var mdgriffith$elm_ui$Internal$Model$isContent = function (len) {
	isContent:
	while (true) {
		switch (len.$) {
			case 1:
				return true;
			case 4:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isContent;
			case 3:
				var l = len.b;
				var $temp$len = l;
				len = $temp$len;
				continue isContent;
			default:
				return false;
		}
	}
};
var elm$core$Basics$always = F2(
	function (a, _n0) {
		return a;
	});
var mdgriffith$elm_ui$Internal$Model$unstyled = A2(elm$core$Basics$composeL, mdgriffith$elm_ui$Internal$Model$Unstyled, elm$core$Basics$always);
var mdgriffith$elm_ui$Element$Input$textHelper = F3(
	function (textInput, attrs, textOptions) {
		var forNearby = function (attr) {
			if (attr.$ === 9) {
				return true;
			} else {
				return false;
			}
		};
		var behavior = _List_fromArray(
			[
				mdgriffith$elm_ui$Internal$Model$Attr(
				elm$html$Html$Events$onInput(textOptions.cm))
			]);
		var attributes = _Utils_ap(mdgriffith$elm_ui$Element$Input$defaultTextBoxStyle, attrs);
		var attributesFromChild = A2(
			mdgriffith$elm_ui$Internal$Model$get,
			attributes,
			function (attr) {
				_n21$7:
				while (true) {
					switch (attr.$) {
						case 7:
							if (attr.a.$ === 2) {
								return true;
							} else {
								break _n21$7;
							}
						case 8:
							if (attr.a.$ === 2) {
								return true;
							} else {
								break _n21$7;
							}
						case 6:
							return true;
						case 5:
							return true;
						case 4:
							switch (attr.b.$) {
								case 5:
									var _n22 = attr.b;
									return true;
								case 2:
									return true;
								case 1:
									var _n23 = attr.b;
									return true;
								default:
									break _n21$7;
							}
						default:
							break _n21$7;
					}
				}
				return false;
			});
		var forPlaceholder = A2(elm$core$List$filter, mdgriffith$elm_ui$Element$Input$inheritablePlaceholderAttributes, attributes);
		var heightFillFromChild = A2(
			mdgriffith$elm_ui$Internal$Model$get,
			attributes,
			function (attr) {
				if ((attr.$ === 8) && (attr.a.$ === 2)) {
					return true;
				} else {
					return false;
				}
			});
		var inputPadding = A2(
			mdgriffith$elm_ui$Internal$Model$get,
			attributes,
			function (attr) {
				if ((attr.$ === 4) && (attr.b.$ === 7)) {
					var _n19 = attr.b;
					return true;
				} else {
					return false;
				}
			});
		var nearbys = A2(
			mdgriffith$elm_ui$Internal$Model$get,
			attributes,
			function (attr) {
				if (attr.$ === 9) {
					return true;
				} else {
					return false;
				}
			});
		var noNearbys = A2(
			elm$core$List$filter,
			A2(elm$core$Basics$composeL, elm$core$Basics$not, forNearby),
			attributes);
		var _n0 = function () {
			var _n1 = textInput.I;
			if (!_n1.$) {
				var inputType = _n1.a;
				return _Utils_Tuple3(
					'input',
					_Utils_ap(
						_List_fromArray(
							[
								mdgriffith$elm_ui$Element$Input$value(textOptions.cR),
								mdgriffith$elm_ui$Internal$Model$Attr(
								elm$html$Html$Attributes$type_(inputType)),
								mdgriffith$elm_ui$Element$Input$spellcheck(textInput.F),
								mdgriffith$elm_ui$Internal$Model$htmlClass(mdgriffith$elm_ui$Internal$Style$classes.eb),
								function () {
								var _n2 = textInput.E;
								if (_n2.$ === 1) {
									return mdgriffith$elm_ui$Internal$Model$NoAttribute;
								} else {
									var fill = _n2.a;
									return mdgriffith$elm_ui$Element$Input$autofill(fill);
								}
							}()
							]),
						noNearbys),
					_List_Nil);
			} else {
				var _n3 = A3(
					elm$core$List$foldr,
					F2(
						function (attr, found) {
							_n4$5:
							while (true) {
								switch (attr.$) {
									case 2:
										return found;
									case 8:
										var len = attr.a;
										var _n5 = found.a4;
										if (_n5.$ === 1) {
											return _Utils_update(
												found,
												{
													y: A2(elm$core$List$cons, attr, found.y),
													a4: elm$core$Maybe$Just(
														mdgriffith$elm_ui$Internal$Model$isContent(len))
												});
										} else {
											return found;
										}
									case 4:
										switch (attr.b.$) {
											case 6:
												var _n6 = attr.b;
												var t = _n6.b;
												var r = _n6.c;
												var b = _n6.d;
												var l = _n6.e;
												var _n7 = found.a6;
												if (_n7.$ === 1) {
													return _Utils_update(
														found,
														{
															y: A2(elm$core$List$cons, attr, found.y),
															a6: elm$core$Maybe$Just(
																A4(mdgriffith$elm_ui$Element$Input$Padding, t, r, b, l))
														});
												} else {
													return found;
												}
											case 7:
												var _n8 = attr.b;
												var t = _n8.b;
												var r = _n8.c;
												var b = _n8.d;
												var l = _n8.e;
												var _n9 = found.a7;
												if (_n9.$ === 1) {
													return _Utils_update(
														found,
														{
															y: found.y,
															a7: elm$core$Maybe$Just(
																A4(mdgriffith$elm_ui$Element$Input$Padding, t, r, b, l))
														});
												} else {
													return found;
												}
											case 5:
												var _n10 = attr.b;
												var x = _n10.b;
												var y = _n10.c;
												var _n11 = found.a8;
												if (_n11.$ === 1) {
													return _Utils_update(
														found,
														{
															y: A2(elm$core$List$cons, attr, found.y),
															a8: elm$core$Maybe$Just(y)
														});
												} else {
													return found;
												}
											default:
												break _n4$5;
										}
									default:
										break _n4$5;
								}
							}
							return _Utils_update(
								found,
								{
									y: A2(elm$core$List$cons, attr, found.y)
								});
						}),
					{y: _List_Nil, a4: elm$core$Maybe$Nothing, a6: elm$core$Maybe$Nothing, a7: elm$core$Maybe$Nothing, a8: elm$core$Maybe$Nothing},
					attributes);
				var maybePadding = _n3.a7;
				var heightContent = _n3.a4;
				var maybeSpacing = _n3.a8;
				var adjustedAttributes = _n3.y;
				var maybeBorder = _n3.a6;
				var spacing = A2(elm$core$Maybe$withDefault, 5, maybeSpacing);
				return _Utils_Tuple3(
					'textarea',
					_Utils_ap(
						_List_fromArray(
							[
								mdgriffith$elm_ui$Element$Input$spellcheck(textInput.F),
								mdgriffith$elm_ui$Internal$Model$htmlClass(mdgriffith$elm_ui$Internal$Style$classes.ea),
								A2(
								elm$core$Maybe$withDefault,
								mdgriffith$elm_ui$Internal$Model$NoAttribute,
								A2(elm$core$Maybe$map, mdgriffith$elm_ui$Element$Input$autofill, textInput.E)),
								function () {
								if (maybePadding.$ === 1) {
									return mdgriffith$elm_ui$Internal$Model$NoAttribute;
								} else {
									var _n13 = maybePadding.a;
									var t = _n13.a;
									var r = _n13.b;
									var b = _n13.c;
									var l = _n13.d;
									return mdgriffith$elm_ui$Element$paddingEach(
										{
											aq: A2(elm$core$Basics$max, 0, b - ((spacing / 2) | 0)),
											at: l,
											az: r,
											aB: A2(elm$core$Basics$max, 0, t - ((spacing / 2) | 0))
										});
								}
							}(),
								function () {
								if (heightContent.$ === 1) {
									return mdgriffith$elm_ui$Internal$Model$NoAttribute;
								} else {
									if (heightContent.a) {
										return A4(mdgriffith$elm_ui$Element$Input$textHeightContent, textOptions.cR, spacing, maybePadding, maybeBorder);
									} else {
										return mdgriffith$elm_ui$Internal$Model$NoAttribute;
									}
								}
							}()
							]),
						adjustedAttributes),
					_List_fromArray(
						[
							mdgriffith$elm_ui$Internal$Model$unstyled(
							elm$html$Html$text(textOptions.cR))
						]));
			}
		}();
		var inputNode = _n0.a;
		var inputAttrs = _n0.b;
		var inputChildren = _n0.c;
		var inputElement = A4(
			mdgriffith$elm_ui$Internal$Model$element,
			mdgriffith$elm_ui$Internal$Model$asEl,
			mdgriffith$elm_ui$Internal$Model$div,
			A2(
				elm$core$List$cons,
				mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$fill),
				elm$core$List$concat(
					_List_fromArray(
						[
							nearbys,
							heightFillFromChild,
							function () {
							var _n15 = textOptions.cu;
							if (_n15.$ === 1) {
								return _List_Nil;
							} else {
								var _n16 = _n15.a;
								var placeholderAttrs = _n16.a;
								var placeholderEl = _n16.b;
								return _List_fromArray(
									[
										mdgriffith$elm_ui$Element$inFront(
										A2(
											mdgriffith$elm_ui$Element$el,
											A2(
												elm$core$List$cons,
												mdgriffith$elm_ui$Element$Input$defaultTextPadding,
												_Utils_ap(
													forPlaceholder,
													_Utils_ap(
														_List_fromArray(
															[
																mdgriffith$elm_ui$Element$Font$color(mdgriffith$elm_ui$Element$Input$charcoal),
																mdgriffith$elm_ui$Internal$Model$htmlClass(mdgriffith$elm_ui$Internal$Style$classes.ck + (' ' + mdgriffith$elm_ui$Internal$Style$classes.eE)),
																mdgriffith$elm_ui$Element$Border$color(
																A4(mdgriffith$elm_ui$Element$rgba, 0, 0, 0, 0)),
																mdgriffith$elm_ui$Element$Background$color(
																A4(mdgriffith$elm_ui$Element$rgba, 0, 0, 0, 0)),
																mdgriffith$elm_ui$Element$height(mdgriffith$elm_ui$Element$fill),
																mdgriffith$elm_ui$Element$width(mdgriffith$elm_ui$Element$fill),
																mdgriffith$elm_ui$Element$alpha(
																(textOptions.cR === '') ? 1 : 0)
															]),
														placeholderAttrs))),
											placeholderEl))
									]);
							}
						}()
						]))),
			mdgriffith$elm_ui$Internal$Model$Unkeyed(
				_List_fromArray(
					[
						A4(
						mdgriffith$elm_ui$Internal$Model$element,
						mdgriffith$elm_ui$Internal$Model$asEl,
						mdgriffith$elm_ui$Internal$Model$NodeName(inputNode),
						elm$core$List$concat(
							_List_fromArray(
								[
									_List_fromArray(
									[
										mdgriffith$elm_ui$Element$Input$focusDefault(attrs),
										mdgriffith$elm_ui$Element$Input$hiddenLabelAttribute(textOptions.ef)
									]),
									inputAttrs,
									behavior
								])),
						mdgriffith$elm_ui$Internal$Model$Unkeyed(inputChildren))
					])));
		return A3(
			mdgriffith$elm_ui$Element$Input$applyLabel,
			A2(
				elm$core$List$cons,
				A2(mdgriffith$elm_ui$Internal$Model$Class, mdgriffith$elm_ui$Internal$Flag$cursor, mdgriffith$elm_ui$Internal$Style$classes.dH),
				A2(
					elm$core$List$cons,
					mdgriffith$elm_ui$Element$Input$isHiddenLabel(textOptions.ef) ? mdgriffith$elm_ui$Internal$Model$NoAttribute : mdgriffith$elm_ui$Element$spacing(5),
					A2(elm$core$List$cons, mdgriffith$elm_ui$Element$Region$announce, attributesFromChild))),
			textOptions.ef,
			inputElement);
	});
var mdgriffith$elm_ui$Element$Input$text = mdgriffith$elm_ui$Element$Input$textHelper(
	{
		E: elm$core$Maybe$Nothing,
		F: false,
		I: mdgriffith$elm_ui$Element$Input$TextInputNode('text')
	});
var elm$virtual_dom$VirtualDom$lazy3 = _VirtualDom_lazy3;
var mdgriffith$elm_ui$Element$Lazy$embed = function (x) {
	switch (x.$) {
		case 0:
			var html = x.a;
			return html;
		case 1:
			var styled = x.a;
			return styled.d4(
				A2(
					mdgriffith$elm_ui$Internal$Model$OnlyDynamic,
					{
						dY: {dd: elm$core$Maybe$Nothing, dm: elm$core$Maybe$Nothing, e$: elm$core$Maybe$Nothing},
						d3: 1,
						eo: 1
					},
					styled.fe));
		case 2:
			var text = x.a;
			return elm$core$Basics$always(
				elm$virtual_dom$VirtualDom$text(text));
		default:
			return elm$core$Basics$always(
				elm$virtual_dom$VirtualDom$text(''));
	}
};
var mdgriffith$elm_ui$Element$Lazy$apply1 = F2(
	function (fn, a) {
		return mdgriffith$elm_ui$Element$Lazy$embed(
			fn(a));
	});
var mdgriffith$elm_ui$Element$Lazy$lazy = F2(
	function (fn, a) {
		return mdgriffith$elm_ui$Internal$Model$Unstyled(
			A3(elm$virtual_dom$VirtualDom$lazy3, mdgriffith$elm_ui$Element$Lazy$apply1, fn, a));
	});
var elm$virtual_dom$VirtualDom$lazy4 = _VirtualDom_lazy4;
var mdgriffith$elm_ui$Element$Lazy$apply2 = F3(
	function (fn, a, b) {
		return mdgriffith$elm_ui$Element$Lazy$embed(
			A2(fn, a, b));
	});
var mdgriffith$elm_ui$Element$Lazy$lazy2 = F3(
	function (fn, a, b) {
		return mdgriffith$elm_ui$Internal$Model$Unstyled(
			A4(elm$virtual_dom$VirtualDom$lazy4, mdgriffith$elm_ui$Element$Lazy$apply2, fn, a, b));
	});
var author$project$Main$view = function (_n0) {
	var model = _n0.a;
	var uiModel = _n0.b;
	var spinnerRow = A2(
		mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[
				author$project$Helpers$ElmStyleShotcuts$wf,
				mdgriffith$elm_ui$Element$htmlAttribute(
				A2(elm$html$Html$Attributes$style, 'z-index', '45')),
				mdgriffith$elm_ui$Element$htmlAttribute(
				A2(elm$html$Html$Attributes$style, 'position', 'fixed')),
				mdgriffith$elm_ui$Element$alpha(0.45),
				author$project$Helpers$ElmStyleShotcuts$spy(0)
			]),
		_List_fromArray(
			[
				A2(
				mdgriffith$elm_ui$Element$row,
				_List_fromArray(
					[
						author$project$Helpers$ElmStyleShotcuts$wf,
						author$project$Helpers$ElmStyleShotcuts$hpx(1750),
						author$project$Helpers$Colors$greyBg(75)
					]),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Element$image,
						_List_fromArray(
							[
								author$project$Helpers$ElmStyleShotcuts$eacx,
								author$project$Helpers$ElmStyleShotcuts$eacy,
								author$project$Helpers$ElmStyleShotcuts$bw(0),
								author$project$Helpers$ElmStyleShotcuts$hf,
								author$project$Helpers$ElmStyleShotcuts$pdt(150)
							]),
						{aI: 'Logo', aP: 'red_loader.gif'})
					]))
			]));
	var loaderIconElement = uiModel.ca ? ((elm$core$List$length(model.aC) > 0) ? mdgriffith$elm_ui$Element$none : A2(
		mdgriffith$elm_ui$Element$image,
		_List_fromArray(
			[
				author$project$Helpers$ElmStyleShotcuts$hpx(18),
				author$project$Helpers$ElmStyleShotcuts$bw(author$project$Helpers$ElmUI$one),
				author$project$Helpers$ElmStyleShotcuts$wf,
				author$project$Helpers$ElmStyleShotcuts$pdl(5),
				author$project$Helpers$ElmStyleShotcuts$bwb(2),
				author$project$Helpers$ElmStyleShotcuts$eat
			]),
		{aI: 'Logo', aP: 'loader.gif'})) : A2(
		mdgriffith$elm_ui$Element$row,
		_List_fromArray(
			[
				author$project$Helpers$ElmStyleShotcuts$wf,
				author$project$Helpers$ElmStyleShotcuts$spx(5)
			]),
		_List_fromArray(
			[
				A2(
				mdgriffith$elm_ui$Element$column,
				_List_fromArray(
					[author$project$Helpers$ElmStyleShotcuts$wf]),
				_List_fromArray(
					[
						author$project$Helpers$ElmUI$textValue('No trucks found')
					])),
				A2(
				mdgriffith$elm_ui$Element$column,
				_List_fromArray(
					[
						author$project$Helpers$ElmStyleShotcuts$bw(0),
						author$project$Helpers$ElmStyleShotcuts$wpx(25),
						author$project$Helpers$ElmStyleShotcuts$hpx(25),
						author$project$Helpers$Colors$greyBg(15),
						author$project$Helpers$Colors$greyFont(250)
					]),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Element$Input$button,
						_List_fromArray(
							[author$project$Helpers$ElmStyleShotcuts$wf]),
						{
							ef: A2(
								mdgriffith$elm_ui$Element$el,
								_List_fromArray(
									[author$project$Helpers$ElmStyleShotcuts$eacx]),
								author$project$Helpers$ElmUI$textValue('x')),
							eu: elm$core$Maybe$Just(author$project$Msg$ClearSearchTextAndContinue)
						})
					]))
			]));
	var focusStyle = mdgriffith$elm_ui$Element$focusStyle(
		{dd: elm$core$Maybe$Nothing, dm: elm$core$Maybe$Nothing, e$: elm$core$Maybe$Nothing});
	var _n1 = (elm$core$String$length(
		elm$core$String$trim(uiModel.M)) > 0) ? _Utils_Tuple2(
		_List_fromArray(
			[
				A3(author$project$Helpers$ElmStyleShotcuts$bc, 226, 63, 63),
				A3(author$project$Helpers$ElmStyleShotcuts$fc, 250, 250, 250)
			]),
		A2(
			mdgriffith$elm_ui$Element$image,
			_List_Nil,
			{aI: 'Logo', aP: 'srch_white.ico'})) : _Utils_Tuple2(
		_List_fromArray(
			[
				A3(author$project$Helpers$ElmStyleShotcuts$bc, 198, 201, 206),
				A3(author$project$Helpers$ElmStyleShotcuts$fc, 245, 245, 245)
			]),
		A2(
			mdgriffith$elm_ui$Element$image,
			_List_Nil,
			{aI: 'Logo', aP: 'srch_grey.ico'}));
	var searchStringBtnStyle = _n1.a;
	var searchBtnIcon = _n1.b;
	var navBar = A2(
		mdgriffith$elm_ui$Element$column,
		_List_fromArray(
			[
				author$project$Helpers$ElmStyleShotcuts$wf,
				mdgriffith$elm_ui$Element$htmlAttribute(
				A2(elm$html$Html$Attributes$style, 'z-index', '40')),
				mdgriffith$elm_ui$Element$htmlAttribute(
				A2(elm$html$Html$Attributes$style, 'position', 'fixed')),
				mdgriffith$elm_ui$Element$alpha(1.95),
				author$project$Helpers$ElmStyleShotcuts$spy(0)
			]),
		_List_fromArray(
			[
				A2(
				mdgriffith$elm_ui$Element$row,
				_List_fromArray(
					[
						author$project$Helpers$ElmStyleShotcuts$wf,
						author$project$Helpers$ElmStyleShotcuts$spx(15),
						author$project$Helpers$Colors$greyBg(245)
					]),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Element$row,
						_List_fromArray(
							[
								author$project$Helpers$ElmStyleShotcuts$wpx(305),
								author$project$Helpers$ElmStyleShotcuts$bw(0)
							]),
						_List_fromArray(
							[
								A2(
								mdgriffith$elm_ui$Element$image,
								_List_fromArray(
									[
										author$project$Helpers$ElmStyleShotcuts$hpx(35)
									]),
								{aI: 'Logo', aP: 'mhclogo.png'}),
								A2(
								mdgriffith$elm_ui$Element$el,
								_List_fromArray(
									[
										author$project$Helpers$ElmStyleShotcuts$pdl(5),
										author$project$Helpers$ElmStyleShotcuts$fs(18),
										author$project$Helpers$ElmStyleShotcuts$eab
									]),
								author$project$Helpers$ElmUI$textValue('v2.0.0 - Live'))
							])),
						A2(
						mdgriffith$elm_ui$Element$row,
						_List_fromArray(
							[author$project$Helpers$ElmStyleShotcuts$wf]),
						_List_fromArray(
							[
								A2(
								mdgriffith$elm_ui$Element$row,
								_List_fromArray(
									[
										author$project$Helpers$ElmStyleShotcuts$wpx(350),
										author$project$Helpers$ElmStyleShotcuts$bw(1),
										author$project$Helpers$Colors$greyBorder(200),
										mdgriffith$elm_ui$Element$below(
										uiModel.b4 ? A2(
											mdgriffith$elm_ui$Element$row,
											_List_fromArray(
												[
													A4(author$project$Helpers$ElmStyleShotcuts$pde, 5, 15, 5, 0),
													author$project$Helpers$ElmStyleShotcuts$bw(1),
													A3(author$project$Helpers$ElmStyleShotcuts$bc, 255, 153, 153)
												]),
											_List_fromArray(
												[
													A2(
													mdgriffith$elm_ui$Element$el,
													_List_fromArray(
														[
															author$project$Helpers$ElmStyleShotcuts$pd(15)
														]),
													author$project$Helpers$ElmUI$textValue(uiModel.fL)),
													A2(
													mdgriffith$elm_ui$Element$row,
													_List_fromArray(
														[
															author$project$Helpers$ElmStyleShotcuts$bw(1),
															author$project$Helpers$Colors$greyBg(228)
														]),
													_List_fromArray(
														[
															A2(
															mdgriffith$elm_ui$Element$Input$button,
															_List_Nil,
															{
																ef: author$project$Helpers$ElmUI$textValue(' x '),
																eu: elm$core$Maybe$Just(
																	author$project$Msg$CloseUserWarningsDialog(false))
															})
														]))
												])) : mdgriffith$elm_ui$Element$none)
									]),
								_List_fromArray(
									[
										A2(
										mdgriffith$elm_ui$Element$Input$text,
										_List_fromArray(
											[
												author$project$Helpers$ElmStyleShotcuts$bw(0),
												mdgriffith$elm_ui$Element$htmlAttribute(
												elm_community$html_extra$Html$Events$Extra$onEnter(author$project$Msg$HandleKeyboardEvent))
											]),
										{
											ef: A2(mdgriffith$elm_ui$Element$Input$labelLeft, _List_Nil, mdgriffith$elm_ui$Element$none),
											cm: author$project$Msg$SearchString,
											cu: elm$core$Maybe$Just(
												A2(
													mdgriffith$elm_ui$Element$Input$placeholder,
													_List_fromArray(
														[
															author$project$Helpers$ElmStyleShotcuts$fs(14)
														]),
													A2(
														mdgriffith$elm_ui$Element$el,
														_List_fromArray(
															[author$project$Helpers$ElmStyleShotcuts$eacy]),
														author$project$Helpers$ElmUI$textValue('Keyword truck Search')))),
											cR: uiModel.M
										}),
										A2(
										mdgriffith$elm_ui$Element$Input$button,
										_Utils_ap(
											_List_fromArray(
												[author$project$Helpers$ElmStyleShotcuts$hf]),
											searchStringBtnStyle),
										{
											ef: A2(
												mdgriffith$elm_ui$Element$el,
												_List_fromArray(
													[
														A4(author$project$Helpers$ElmStyleShotcuts$pde, 0, 5, 0, 5)
													]),
												author$project$Helpers$ElmUI$textValue('SEARCH')),
											eu: (elm$core$String$length(uiModel.M) > 0) ? elm$core$Maybe$Just(author$project$Msg$SearchPressed) : elm$core$Maybe$Nothing
										})
									])),
								A2(
								mdgriffith$elm_ui$Element$row,
								_List_fromArray(
									[
										author$project$Helpers$ElmStyleShotcuts$ear,
										author$project$Helpers$ElmStyleShotcuts$spx(15),
										author$project$Helpers$ElmStyleShotcuts$fs(18),
										author$project$Helpers$ElmStyleShotcuts$pdl(15)
									]),
								_List_fromArray(
									[
										A2(
										mdgriffith$elm_ui$Element$row,
										_List_fromArray(
											[
												author$project$Helpers$ElmStyleShotcuts$bwl(0),
												author$project$Helpers$ElmStyleShotcuts$hpx(30)
											]),
										_List_Nil),
										A2(
										mdgriffith$elm_ui$Element$row,
										_List_fromArray(
											[
												author$project$Helpers$ElmStyleShotcuts$wpx(325),
												A3(author$project$Helpers$ElmStyleShotcuts$fc, 97, 97, 97),
												mdgriffith$elm_ui$Element$below(
												A2(author$project$TruckViews$SortDialog$showSortOptionsDialog, uiModel.bE, uiModel.aH))
											]),
										_List_fromArray(
											[
												A2(
												mdgriffith$elm_ui$Element$Input$button,
												_List_Nil,
												{
													ef: A2(
														mdgriffith$elm_ui$Element$el,
														_List_fromArray(
															[
																author$project$Helpers$ElmStyleShotcuts$bwb(1)
															]),
														author$project$Helpers$ElmUI$textValue('Sort by : ' + uiModel.aH._)),
													eu: elm$core$Maybe$Just(
														author$project$Msg$OperateSortDialog(!uiModel.bE))
												})
											]))
									]))
							]))
					])),
				A2(
				mdgriffith$elm_ui$Element$row,
				_List_fromArray(
					[
						author$project$Helpers$ElmStyleShotcuts$wf,
						author$project$Helpers$Colors$greyBg(235),
						mdgriffith$elm_ui$Element$Border$shadow(
						{
							di: 15.0,
							dB: A3(mdgriffith$elm_ui$Element$rgb255, 185, 185, 185),
							es: _Utils_Tuple2(0, 3),
							e1: 1
						}),
						A2(author$project$Helpers$ElmStyleShotcuts$hfRange, 40, 65),
						mdgriffith$elm_ui$Element$clipY
					]),
				_List_fromArray(
					[
						A2(
						mdgriffith$elm_ui$Element$column,
						_List_fromArray(
							[
								author$project$Helpers$ElmStyleShotcuts$bw(0)
							]),
						_List_fromArray(
							[
								A2(
								mdgriffith$elm_ui$Element$row,
								_List_fromArray(
									[
										author$project$Helpers$ElmStyleShotcuts$wpx(315),
										author$project$Helpers$ElmStyleShotcuts$spx(15),
										author$project$Helpers$ElmStyleShotcuts$fs(12),
										author$project$Helpers$Colors$mhcRed,
										author$project$Helpers$ElmStyleShotcuts$pdl(15)
									]),
								_List_fromArray(
									[
										A2(
										mdgriffith$elm_ui$Element$Input$button,
										_List_fromArray(
											[
												mdgriffith$elm_ui$Element$mouseOver(
												_List_fromArray(
													[
														A3(author$project$Helpers$ElmStyleShotcuts$fc, 217, 98, 69)
													]))
											]),
										{
											ef: A2(
												mdgriffith$elm_ui$Element$el,
												_List_fromArray(
													[
														author$project$Helpers$ElmStyleShotcuts$bwb(1)
													]),
												author$project$Helpers$ElmUI$textValue('EXPAND ALL')),
											eu: elm$core$Maybe$Just(
												author$project$Msg$CollapseAllClicked(true))
										}),
										A2(
										mdgriffith$elm_ui$Element$Input$button,
										_List_fromArray(
											[
												mdgriffith$elm_ui$Element$mouseOver(
												_List_fromArray(
													[
														A3(author$project$Helpers$ElmStyleShotcuts$fc, 217, 98, 69)
													]))
											]),
										{
											ef: A2(
												mdgriffith$elm_ui$Element$el,
												_List_fromArray(
													[
														author$project$Helpers$ElmStyleShotcuts$bwb(1)
													]),
												author$project$Helpers$ElmUI$textValue('COLLAPSE ALL')),
											eu: elm$core$Maybe$Just(
												author$project$Msg$CollapseAllClicked(false))
										}),
										A2(
										mdgriffith$elm_ui$Element$Input$button,
										_List_fromArray(
											[
												mdgriffith$elm_ui$Element$mouseOver(
												_List_fromArray(
													[
														A3(author$project$Helpers$ElmStyleShotcuts$fc, 217, 98, 69)
													]))
											]),
										{
											ef: A2(
												mdgriffith$elm_ui$Element$el,
												_List_fromArray(
													[
														author$project$Helpers$ElmStyleShotcuts$bwb(1)
													]),
												author$project$Helpers$ElmUI$textValue('CLEAR FILTERS')),
											eu: (author$project$BusinessFunctions$SearchFilterFunctions$anyFilterApplied(uiModel) || (uiModel.M !== '')) ? elm$core$Maybe$Just(author$project$Msg$ClearAllFilters) : elm$core$Maybe$Nothing
										})
									]))
							])),
						A2(
						mdgriffith$elm_ui$Element$column,
						_List_fromArray(
							[
								author$project$Helpers$ElmStyleShotcuts$bwr(1),
								author$project$Helpers$ElmStyleShotcuts$hf,
								author$project$Helpers$ElmStyleShotcuts$pdr(25)
							]),
						_List_fromArray(
							[
								A2(
								mdgriffith$elm_ui$Element$el,
								_List_fromArray(
									[
										author$project$Helpers$Colors$mhcRed,
										author$project$Helpers$ElmStyleShotcuts$pdr(0),
										author$project$Helpers$ElmStyleShotcuts$eacy
									]),
								author$project$Helpers$ElmUI$textValue(
									'Total trucks found : ' + elm$core$String$fromInt(model.bH)))
							])),
						A2(
						mdgriffith$elm_ui$Element$column,
						_List_fromArray(
							[
								author$project$Helpers$ElmStyleShotcuts$wfp(2),
								author$project$Helpers$ElmStyleShotcuts$pdl(15)
							]),
						_List_fromArray(
							[
								A2(
								mdgriffith$elm_ui$Element$wrappedRow,
								_List_fromArray(
									[
										author$project$Helpers$ElmStyleShotcuts$wf,
										author$project$Helpers$ElmStyleShotcuts$pd(8),
										author$project$Helpers$ElmStyleShotcuts$spx(5),
										author$project$Helpers$ElmStyleShotcuts$spy(5)
									]),
								A2(author$project$BusinessFunctions$Pager$buildPageNumbersView, uiModel.bj, uiModel.U))
							]))
					]))
			]));
	return A3(
		mdgriffith$elm_ui$Element$layoutWith,
		{
			ez: _List_fromArray(
				[focusStyle])
		},
		_List_fromArray(
			[
				mdgriffith$elm_ui$Element$Font$family(
				_List_fromArray(
					[
						mdgriffith$elm_ui$Element$Font$external(
						{eq: 'Roboto', fJ: 'https://fonts.googleapis.com/css?family=Roboto'}),
						mdgriffith$elm_ui$Element$Font$sansSerif
					]))
			]),
		A2(
			mdgriffith$elm_ui$Element$column,
			_List_fromArray(
				[
					author$project$Helpers$ElmStyleShotcuts$wfmax(1920)
				]),
			_List_fromArray(
				[
					navBar,
					A2(
					mdgriffith$elm_ui$Element$row,
					_List_fromArray(
						[
							author$project$Helpers$ElmStyleShotcuts$wf,
							A4(author$project$Helpers$ElmStyleShotcuts$pde, 100, 3, 0, 3),
							author$project$Helpers$ElmStyleShotcuts$spx(16)
						]),
					_List_fromArray(
						[
							A2(
							mdgriffith$elm_ui$Element$column,
							_List_fromArray(
								[
									author$project$Helpers$ElmStyleShotcuts$wpx(300),
									author$project$Helpers$ElmStyleShotcuts$eat
								]),
							_List_fromArray(
								[
									A2(
									mdgriffith$elm_ui$Element$column,
									_List_fromArray(
										[
											author$project$Helpers$ElmStyleShotcuts$wf,
											author$project$Helpers$ElmStyleShotcuts$spy(5),
											author$project$Helpers$Colors$greyBg(240),
											author$project$Helpers$ElmStyleShotcuts$pd(10),
											author$project$Helpers$ElmStyleShotcuts$bw(0),
											author$project$Helpers$ElmStyleShotcuts$hf,
											mdgriffith$elm_ui$Element$inFront(
											uiModel.O ? spinnerRow : mdgriffith$elm_ui$Element$none)
										]),
									(!elm$core$List$length(model.aC)) ? _List_fromArray(
										[loaderIconElement]) : A2(
										elm$core$List$map,
										function (filterType) {
											return A3(
												mdgriffith$elm_ui$Element$Lazy$lazy2,
												author$project$SearchFilterViews$SearchFilter$buildSearchFilterValuesGroup,
												{
													eW: filterType.i,
													eY: A2(
														elm$core$Array$filter,
														function (sState) {
															return _Utils_eq(sState.eV, filterType.h);
														},
														uiModel.aJ)
												},
												A2(
													elm$core$List$filter,
													function (f) {
														return _Utils_eq(f.dV, filterType.h);
													},
													uiModel.ao));
										},
										author$project$BusinessFunctions$SearchFilterFunctions$partialSearchFiltersMetadata))
								])),
							A2(
							mdgriffith$elm_ui$Element$column,
							_List_fromArray(
								[author$project$Helpers$ElmStyleShotcuts$wf, author$project$Helpers$ElmStyleShotcuts$hf]),
							_List_fromArray(
								[
									A2(mdgriffith$elm_ui$Element$Lazy$lazy, author$project$TruckViews$SearchFilterBullet$searchFilterBulletView, uiModel.bD),
									A2(
									mdgriffith$elm_ui$Element$column,
									_List_fromArray(
										[author$project$Helpers$ElmStyleShotcuts$wf]),
									_List_fromArray(
										[
											A2(mdgriffith$elm_ui$Element$Lazy$lazy, author$project$TruckViews$Truck$trucksView, model.aC)
										]))
								]))
						]))
				])));
};
var elm$browser$Browser$element = _Browser_element;
var author$project$Main$main = elm$browser$Browser$element(
	{d9: author$project$Main$init, fg: author$project$Main$subscriptions, fI: author$project$Main$update, fM: author$project$Main$view});
_Platform_export({'Main':{'init':author$project$Main$main(elm$json$Json$Decode$string)(0)}});}(this));