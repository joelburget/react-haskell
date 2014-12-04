// EventProperties
function js_bubbles(obj) {return obj.bubbles;}
function js_cancelable(obj) {return obj.cancelable;}
function js_currentTarget(obj) {return obj.currentTarget;}
function js_defaultPrevented(obj) {return obj.defaultPrevented;}
function js_eventPhase(obj) {return obj.eventPhase;}
function js_isTrusted(obj) {return obj.isTrusted;}
//                  -- ,  nativeEvent :: DOMEvent
//                  -- , preventDefault :: IO ()
//                  -- ,  stopPropagation :: IO ()
function js_evtTarget(obj) {return obj.evtTarget;}
//                  --, timeStamp :: DateTime

//ModifierKeys
function js_type(obj) {return obj.type;}
function js_altKey(obj) {return obj.altKey;}
function js_ctrlKey(obj) {return obj.ctrlKey;}
function js_metaKey(obj) {return obj.metaKey;}
function js_shiftKey(obj) {return obj.shiftKey;}

//MouseEvent
function js_button(obj) {return obj.button;}
function js_buttons(obj) {return obj.buttons;}
function js_clientX(obj) {return obj.clientX;}
function js_clientY(obj) {return obj.clientY;}
function js_pageX(obj) {return obj.pageX;}
function js_pageY(obj) {return obj.pageY;}
//--                             , relatedTarget :: Unpacked
function js_screenX(obj) {return obj.screenX;}
function js_screenY(obj) {return obj.screenY;}

// KeyboardEvent
function js_charCode(obj) {return obj.charCode;}
function js_key(obj) {return obj.key;}
function js_keyCode(obj) {return obj.keyCode;}
function js_locale(obj) {return obj.locale;}
function js_location(obj) {return obj.location;}
function js_repeat(obj) {return obj.repeat;}
function js_which(obj) {return obj.which;}

//ChangeEvent
function js_targetValue(obj) {return obj.target.value;}

//FocusEvent
function js_focusEventProperties(obj) {return obj.focusEventProperties;}
function js_domEventTarget(obj) {return obj.domEventTarget;}
function js_relatedTarget(obj) {return obj.relatedTarget;}

function js_empty_object() {return {};}
function js_set_field(obj, field, value) {obj[field] = value;}
function js_set_field_True(obj, field, value) {obj[field] = true;}
function js_set_field_False(obj, field, value) {obj[field] = false;}

function js_set_onClick(cb, attrs) {attrs.onClick = function(e) {B(A(cb,[[0,e],0]));};}
function js_set_onDoubleClick(cb, attrs) {attrs.onDoubleClick = function(e) {B(A(cb,[[0,e],0]));};}
function js_set_onChange(cb, attrs) {attrs.onChange = function(e) {B(A(cb,[[0,e],0]));};}
function js_set_onKeyUp(cb, attrs) {attrs.onKeyUp = function(e) {B(A(cb,[[0,e],0]));};}
function js_set_onKeyPress(cb, attrs) {attrs.onKeyPress = function(e) {B(A(cb,[[0,e],0]));};}
function js_set_onKeyDown(cb, attrs) {attrs.onKeyDown = function(e) { B(A(cb,[[0,e],0]));};}
function js_set_onBlur(cb, attrs) {attrs.onBlur = function(e) {B(A(cb,[[0,e],0]));};}
function js_set_onMouseEnter(cb, attrs) {attrs.onMouseEnter = function(e) {B(A(cb,[[0,e],0]));};}
function js_set_onMouseLeave(cb, attrs) {attrs.onMouseLeave = function(e) {B(A(cb,[[0,e],0]));};}

function js_React_DOM(tagName, attrs) {return React.DOM[tagName](attrs);}

function js_empty() {return [];}
function js_push(a,v) {a.push(v);}

// Ptr (Maybe React)
function js_React_getDomNode(r) {
  try {
    var ret = [1, [0, r.getDOMNode()]];
  } catch(e) {
    return [0];
  }
  return ret;
}

// Normal elements
function js_React_DOM_a(a,c) {return React.DOM.a(a,c);}
function js_React_DOM_abbr(a,c) {return React.DOM.abbr(a,c);}
function js_React_DOM_address(a,c) {return React.DOM.address(a,c);}
function js_React_DOM_article(a,c) {return React.DOM.article(a,c);}
function js_React_DOM_aside(a,c) {return React.DOM.aside(a,c);}
function js_React_DOM_audio(a,c) {return React.DOM.audio(a,c);}
function js_React_DOM_b(a,c) {return React.DOM.b(a,c);}
function js_React_DOM_bdi(a,c) {return React.DOM.bdi(a,c);}
function js_React_DOM_bdo(a,c) {return React.DOM.bdo(a,c);}
function js_React_DOM_big(a,c) {return React.DOM.big(a,c);}
function js_React_DOM_blockquote(a,c) {return React.DOM.blockquote(a,c);}
function js_React_DOM_body(a,c) {return React.DOM.body(a,c);}
function js_React_DOM_button(a,c) {return React.DOM.button(a,c);}
function js_React_DOM_canvas(a,c) {return React.DOM.canvas(a,c);}
function js_React_DOM_caption(a,c) {return React.DOM.caption(a,c);}
function js_React_DOM_cite(a,c) {return React.DOM.cite(a,c);}
function js_React_DOM_code(a,c) {return React.DOM.code(a,c);}
function js_React_DOM_colgroup(a,c) {return React.DOM.colgroup(a,c);}
function js_React_DOM_data(a,c) {return React.DOM.data(a,c);}
function js_React_DOM_datalist(a,c) {return React.DOM.datalist(a,c);}
function js_React_DOM_dd(a,c) {return React.DOM.dd(a,c);}
function js_React_DOM_del(a,c) {return React.DOM.del(a,c);}
function js_React_DOM_details(a,c) {return React.DOM.details(a,c);}
function js_React_DOM_dfn(a,c) {return React.DOM.dfn(a,c);}
function js_React_DOM_div(a,c) {return React.DOM.div(a,c);}
function js_React_DOM_dl(a,c) {return React.DOM.dl(a,c);}
function js_React_DOM_dt(a,c) {return React.DOM.dt(a,c);}
function js_React_DOM_em(a,c) {return React.DOM.em(a,c);}
function js_React_DOM_fieldset(a,c) {return React.DOM.fieldset(a,c);}
function js_React_DOM_figcaption(a,c) {return React.DOM.figcaption(a,c);}
function js_React_DOM_figure(a,c) {return React.DOM.figure(a,c);}
function js_React_DOM_footer(a,c) {return React.DOM.footer(a,c);}
function js_React_DOM_form(a,c) {return React.DOM.form(a,c);}
function js_React_DOM_h1(a,c) {return React.DOM.h1(a,c);}
function js_React_DOM_h2(a,c) {return React.DOM.h2(a,c);}
function js_React_DOM_h3(a,c) {return React.DOM.h3(a,c);}
function js_React_DOM_h4(a,c) {return React.DOM.h4(a,c);}
function js_React_DOM_h5(a,c) {return React.DOM.h5(a,c);}
function js_React_DOM_h6(a,c) {return React.DOM.h6(a,c);}
function js_React_DOM_head(a,c) {return React.DOM.head(a,c);}
function js_React_DOM_header(a,c) {return React.DOM.header(a,c);}
function js_React_DOM_html(a,c) {return React.DOM.html(a,c);}
function js_React_DOM_i(a,c) {return React.DOM.i(a,c);}
function js_React_DOM_iframe(a,c) {return React.DOM.iframe(a,c);}
function js_React_DOM_ins(a,c) {return React.DOM.ins(a,c);}
function js_React_DOM_kbd(a,c) {return React.DOM.kbd(a,c);}
function js_React_DOM_label(a,c) {return React.DOM.label(a,c);}
function js_React_DOM_legend(a,c) {return React.DOM.legend(a,c);}
function js_React_DOM_li(a,c) {return React.DOM.li(a,c);}
function js_React_DOM_main(a,c) {return React.DOM.main(a,c);}
function js_React_DOM_map(a,c) {return React.DOM.map(a,c);}
function js_React_DOM_mark(a,c) {return React.DOM.mark(a,c);}
function js_React_DOM_menu(a,c) {return React.DOM.menu(a,c);}
function js_React_DOM_menuitem(a,c) {return React.DOM.menuitem(a,c);}
function js_React_DOM_meter(a,c) {return React.DOM.meter(a,c);}
function js_React_DOM_nav(a,c) {return React.DOM.nav(a,c);}
function js_React_DOM_noscript(a,c) {return React.DOM.noscript(a,c);}
function js_React_DOM_object(a,c) {return React.DOM.object(a,c);}
function js_React_DOM_ol(a,c) {return React.DOM.ol(a,c);}
function js_React_DOM_optgroup(a,c) {return React.DOM.optgroup(a,c);}
function js_React_DOM_option(a,c) {return React.DOM.option(a,c);}
function js_React_DOM_output(a,c) {return React.DOM.output(a,c);}
function js_React_DOM_p(a,c) {return React.DOM.p(a,c);}
function js_React_DOM_pre(a,c) {return React.DOM.pre(a,c);}
function js_React_DOM_progress(a,c) {return React.DOM.progress(a,c);}
function js_React_DOM_q(a,c) {return React.DOM.q(a,c);}
function js_React_DOM_rp(a,c) {return React.DOM.rp(a,c);}
function js_React_DOM_rt(a,c) {return React.DOM.rt(a,c);}
function js_React_DOM_ruby(a,c) {return React.DOM.ruby(a,c);}
function js_React_DOM_s(a,c) {return React.DOM.s(a,c);}
function js_React_DOM_samp(a,c) {return React.DOM.samp(a,c);}
function js_React_DOM_section(a,c) {return React.DOM.section(a,c);}
function js_React_DOM_select(a,c) {return React.DOM.select(a,c);}
function js_React_DOM_small(a,c) {return React.DOM.small(a,c);}
function js_React_DOM_span(a,c) {return React.DOM.span(a,c);}
function js_React_DOM_strong(a,c) {return React.DOM.strong(a,c);}
function js_React_DOM_sub(a,c) {return React.DOM.sub(a,c);}
function js_React_DOM_summary(a,c) {return React.DOM.summary(a,c);}
function js_React_DOM_sup(a,c) {return React.DOM.sup(a,c);}
function js_React_DOM_table(a,c) {return React.DOM.table(a,c);}
function js_React_DOM_tbody(a,c) {return React.DOM.tbody(a,c);}
function js_React_DOM_td(a,c) {return React.DOM.td(a,c);}
function js_React_DOM_tfoot(a,c) {return React.DOM.tfoot(a,c);}
function js_React_DOM_th(a,c) {return React.DOM.th(a,c);}
function js_React_DOM_thead(a,c) {return React.DOM.thead(a,c);}
function js_React_DOM_time(a,c) {return React.DOM.time(a,c);}
function js_React_DOM_tr(a,c) {return React.DOM.tr(a,c);}
function js_React_DOM_u(a,c) {return React.DOM.u(a,c);}
function js_React_DOM_ul(a,c) {return React.DOM.ul(a,c);}
function js_React_DOM_var(a,c) {return React.DOM.var(a,c);}
function js_React_DOM_video(a,c) {return React.DOM.video(a,c);}

// No-child
function js_React_DOM_area(a) {return React.DOM.area(a);}
function js_React_DOM_base(a) {return React.DOM.base(a);}
function js_React_DOM_br(a) {return React.DOM.br(a);}
function js_React_DOM_col(a) {return React.DOM.col(a);}
function js_React_DOM_embed(a) {return React.DOM.embed(a);}
function js_React_DOM_hr(a) {return React.DOM.hr(a);}
function js_React_DOM_img(a) {return React.DOM.img(a);}
function js_React_DOM_input(a) {return React.DOM.input(a);}
function js_React_DOM_keygen(a) {return React.DOM.keygen(a);}
function js_React_DOM_link(a) {return React.DOM.link(a);}
function js_React_DOM_meta(a) {return React.DOM.meta(a);}
function js_React_DOM_param(a) {return React.DOM.param(a);}
function js_React_DOM_source(a) {return React.DOM.source(a);}
function js_React_DOM_track(a) {return React.DOM.track(a);}
function js_React_DOM_wbr(a) {return React.DOM.wbr(a);}

// text-only
function js_React_DOM_script(a,t) {return React.DOM.script(a,t);}
function js_React_DOM_style(a,t) {return React.DOM.style(a,t);}
function js_React_DOM_textarea(a,t) {return React.DOM.textarea(a,t);}
function js_React_DOM_title(a,t) {return React.DOM.title(a,t);}
function js_id(a) {return a;}

// custom
function js_React_DOM_leaf(name, a) { return React.DOM[name](a); }
function js_React_DOM_parent(name, a, c) { return React.DOM[name](a, c); }

function js_parseChangeEvent(raw) {
    // wrap the string in two constructors - Ptr and JSString
    return [0, raw.target.value];
}

function js_parseKeyboardEvent(raw) {
    return [
        0,
        [
            0,
            raw.altKey,
            raw.ctrlKey,
            raw.metaKey,
            raw.shiftKey,
        ],
        raw.charCode,
        raw.key,
        raw.keyCode,
        raw.locale,
        raw.location,
        raw.repeat,
        raw.which,
    ];
}

function js_parseMouseEvent(raw) {
    return [
        0,
        [
            0,
            raw.altKey,
            raw.ctrlKey,
            raw.metaKey,
            raw.shiftKey,
        ],
        raw.button,
        raw.clientX,
        raw.clientY,
        raw.pageX,
        raw.pageY,
        raw.screenX,
        raw.screenY,
    ];
}

// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = f;
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f != __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            t.x = f();
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;        
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsQuerySelectorAll(elem, query) {
  var els = [0],
      len, nl, i;

  if (!elem || typeof elem.querySelectorAll !== 'function') {
    return els;
  }

  nl = elem.querySelectorAll(query);
  len = nl.length;

  for (i=len-1; i >= 0; --i) {
    els = [1, [0, nl[i]], els];
  }

  return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);})]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_1=new T(function(){return B(unCStr("base"));}),_2=new T(function(){return B(unCStr("IOException"));}),_3=[0],_4=new T(function(){var _5=hs_wordToWord64(4053623282),_6=_5,_7=hs_wordToWord64(3693590983),_8=_7;return [0,_6,_8,[0,_6,_8,_1,_0,_2],_3];}),_9=function(_a){return E(_4);},_b=function(_c){return E(E(_c)[1]);},_d=function(_e,_f,_g){var _h=B(A(_e,[_])),_i=B(A(_f,[_])),_j=hs_eqWord64(_h[1],_i[1]),_k=_j;if(!E(_k)){return [0];}else{var _l=hs_eqWord64(_h[2],_i[2]),_m=_l;return E(_m)==0?[0]:[1,_g];}},_n=function(_o){var _p=E(_o);return new F(function(){return _d(B(_b(_p[1])),_9,_p[2]);});},_q=new T(function(){return B(unCStr(": "));}),_r=[0,41],_s=new T(function(){return B(unCStr(" ("));}),_t=function(_u,_v){var _w=E(_u);return _w[0]==0?E(_v):[1,_w[1],new T(function(){return B(_t(_w[2],_v));})];},_x=new T(function(){return B(unCStr("already exists"));}),_y=new T(function(){return B(unCStr("does not exist"));}),_z=new T(function(){return B(unCStr("protocol error"));}),_A=new T(function(){return B(unCStr("failed"));}),_B=new T(function(){return B(unCStr("invalid argument"));}),_C=new T(function(){return B(unCStr("inappropriate type"));}),_D=new T(function(){return B(unCStr("hardware fault"));}),_E=new T(function(){return B(unCStr("unsupported operation"));}),_F=new T(function(){return B(unCStr("timeout"));}),_G=new T(function(){return B(unCStr("resource vanished"));}),_H=new T(function(){return B(unCStr("interrupted"));}),_I=new T(function(){return B(unCStr("resource busy"));}),_J=new T(function(){return B(unCStr("resource exhausted"));}),_K=new T(function(){return B(unCStr("end of file"));}),_L=new T(function(){return B(unCStr("illegal operation"));}),_M=new T(function(){return B(unCStr("permission denied"));}),_N=new T(function(){return B(unCStr("user error"));}),_O=new T(function(){return B(unCStr("unsatisified constraints"));}),_P=new T(function(){return B(unCStr("system error"));}),_Q=function(_R,_S){switch(E(_R)){case 0:return new F(function(){return _t(_x,_S);});break;case 1:return new F(function(){return _t(_y,_S);});break;case 2:return new F(function(){return _t(_I,_S);});break;case 3:return new F(function(){return _t(_J,_S);});break;case 4:return new F(function(){return _t(_K,_S);});break;case 5:return new F(function(){return _t(_L,_S);});break;case 6:return new F(function(){return _t(_M,_S);});break;case 7:return new F(function(){return _t(_N,_S);});break;case 8:return new F(function(){return _t(_O,_S);});break;case 9:return new F(function(){return _t(_P,_S);});break;case 10:return new F(function(){return _t(_z,_S);});break;case 11:return new F(function(){return _t(_A,_S);});break;case 12:return new F(function(){return _t(_B,_S);});break;case 13:return new F(function(){return _t(_C,_S);});break;case 14:return new F(function(){return _t(_D,_S);});break;case 15:return new F(function(){return _t(_E,_S);});break;case 16:return new F(function(){return _t(_F,_S);});break;case 17:return new F(function(){return _t(_G,_S);});break;default:return new F(function(){return _t(_H,_S);});}},_T=[0,125],_U=new T(function(){return B(unCStr("{handle: "));}),_V=function(_W,_X,_Y,_Z,_10,_11){var _12=new T(function(){var _13=new T(function(){return B(_Q(_X,new T(function(){var _14=E(_Z);return _14[0]==0?E(_11):B(_t(_s,new T(function(){return B(_t(_14,[1,_r,_11]));})));})));}),_15=E(_Y);return _15[0]==0?E(_13):B(_t(_15,new T(function(){return B(_t(_q,_13));})));}),_16=E(_10);if(!_16[0]){var _17=E(_W);if(!_17[0]){return E(_12);}else{var _18=E(_17[1]);return _18[0]==0?B(_t(_U,new T(function(){return B(_t(_18[1],[1,_T,new T(function(){return B(_t(_q,_12));})]));}))):B(_t(_U,new T(function(){return B(_t(_18[1],[1,_T,new T(function(){return B(_t(_q,_12));})]));})));}}else{return new F(function(){return _t(_16[1],new T(function(){return B(_t(_q,_12));}));});}},_19=function(_1a){var _1b=E(_1a);return new F(function(){return _V(_1b[1],_1b[2],_1b[3],_1b[4],_1b[6],_3);});},_1c=function(_1d,_1e){var _1f=E(_1d);return new F(function(){return _V(_1f[1],_1f[2],_1f[3],_1f[4],_1f[6],_1e);});},_1g=[0,44],_1h=[0,93],_1i=[0,91],_1j=function(_1k,_1l,_1m){var _1n=E(_1l);return _1n[0]==0?B(unAppCStr("[]",_1m)):[1,_1i,new T(function(){return B(A(_1k,[_1n[1],new T(function(){var _1o=function(_1p){var _1q=E(_1p);return _1q[0]==0?E([1,_1h,_1m]):[1,_1g,new T(function(){return B(A(_1k,[_1q[1],new T(function(){return B(_1o(_1q[2]));})]));})];};return B(_1o(_1n[2]));})]));})];},_1r=function(_1s,_1t){return new F(function(){return _1j(_1c,_1s,_1t);});},_1u=function(_1v,_1w,_1x){var _1y=E(_1w);return new F(function(){return _V(_1y[1],_1y[2],_1y[3],_1y[4],_1y[6],_1x);});},_1z=[0,_1u,_19,_1r],_1A=new T(function(){return [0,_9,_1z,_1B,_n];}),_1B=function(_1C){return [0,_1A,_1C];},_1D=[0],_1E=7,_1F=function(_1G){return [0,_1D,_1E,_3,_1G,_1D,_1D];},_1H=function(_1I,_){return new F(function(){return die(new T(function(){return B(_1B(new T(function(){return B(_1F(_1I));})));}));});},_1J=function(_1K,_){return new F(function(){return _1H(_1K,_);});},_1L=new T(function(){return B(unCStr("Pattern match failure in do expression at nest.hs:56:5-13"));}),_1M=function(_1N,_1O){var _1P=jsShowI(_1N),_1Q=_1P;return new F(function(){return _t(fromJSStr(_1Q),_1O);});},_1R=[0,41],_1S=[0,40],_1T=function(_1U,_1V,_1W){if(_1V>=0){return new F(function(){return _1M(_1V,_1W);});}else{return _1U<=6?B(_1M(_1V,_1W)):[1,_1S,new T(function(){var _1X=jsShowI(_1V),_1Y=_1X;return B(_t(fromJSStr(_1Y),[1,_1R,_1W]));})];}},_1Z=0,_20=new T(function(){return [0,toJSStr(_3)];}),_21=new T(function(){return [0,"input"];}),_22=0,_23=[2],_24=function(_25){return [3,_25,_23];},_26=new T(function(){return B(unCStr("Control.Exception.Base"));}),_27=new T(function(){return B(unCStr("base"));}),_28=new T(function(){return B(unCStr("PatternMatchFail"));}),_29=new T(function(){var _2a=hs_wordToWord64(18445595),_2b=_2a,_2c=hs_wordToWord64(52003073),_2d=_2c;return [0,_2b,_2d,[0,_2b,_2d,_27,_26,_28],_3];}),_2e=function(_2f){return E(_29);},_2g=function(_2h){var _2i=E(_2h);return new F(function(){return _d(B(_b(_2i[1])),_2e,_2i[2]);});},_2j=function(_2k){return E(E(_2k)[1]);},_2l=function(_2m,_2n){return new F(function(){return _t(E(_2m)[1],_2n);});},_2o=function(_2p,_2q){return new F(function(){return _1j(_2l,_2p,_2q);});},_2r=function(_2s,_2t,_2u){return new F(function(){return _t(E(_2t)[1],_2u);});},_2v=[0,_2r,_2j,_2o],_2w=new T(function(){return [0,_2e,_2v,_2x,_2g];}),_2x=function(_2y){return [0,_2w,_2y];},_2z=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_2A=function(_2B,_2C){return new F(function(){return die(new T(function(){return B(A(_2C,[_2B]));}));});},_2D=function(_2E,_2F){var _2G=E(_2F);if(!_2G[0]){return [0,_3,_3];}else{var _2H=_2G[1];if(!B(A(_2E,[_2H]))){return [0,_3,_2G];}else{var _2I=new T(function(){var _2J=B(_2D(_2E,_2G[2]));return [0,_2J[1],_2J[2]];});return [0,[1,_2H,new T(function(){return E(E(_2I)[1]);})],new T(function(){return E(E(_2I)[2]);})];}}},_2K=[0,32],_2L=[0,10],_2M=[1,_2L,_3],_2N=function(_2O){return E(E(_2O)[1])==124?false:true;},_2P=function(_2Q,_2R){var _2S=B(_2D(_2N,B(unCStr(_2Q)))),_2T=_2S[1],_2U=function(_2V,_2W){return new F(function(){return _t(_2V,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_t(_2R,new T(function(){return B(_t(_2W,_2M));})));})));}));});},_2X=E(_2S[2]);if(!_2X[0]){return new F(function(){return _2U(_2T,_3);});}else{return E(E(_2X[1])[1])==124?B(_2U(_2T,[1,_2K,_2X[2]])):B(_2U(_2T,_3));}},_2Y=function(_2Z){return new F(function(){return _2A([0,new T(function(){return B(_2P(_2Z,_2z));})],_2x);});},_30=new T(function(){return B(_2Y("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_31=function(_32,_33){while(1){var _34=(function(_35,_36){var _37=E(_35);switch(_37[0]){case 0:var _38=E(_36);if(!_38[0]){return [0];}else{_32=B(A(_37[1],[_38[1]]));_33=_38[2];return null;}break;case 1:var _39=B(A(_37[1],[_36])),_3a=_36;_32=_39;_33=_3a;return null;case 2:return [0];case 3:return [1,[0,_37[1],_36],new T(function(){return B(_31(_37[2],_36));})];default:return E(_37[1]);}})(_32,_33);if(_34!=null){return _34;}}},_3b=function(_3c,_3d){var _3e=function(_3f){var _3g=E(_3d);if(_3g[0]==3){return [3,_3g[1],new T(function(){return B(_3b(_3c,_3g[2]));})];}else{var _3h=E(_3c);if(_3h[0]==2){return E(_3g);}else{var _3i=E(_3g);if(_3i[0]==2){return E(_3h);}else{var _3j=function(_3k){var _3l=E(_3i);if(_3l[0]==4){return [1,function(_3m){return [4,new T(function(){return B(_t(B(_31(_3h,_3m)),_3l[1]));})];}];}else{var _3n=E(_3h);if(_3n[0]==1){var _3o=_3n[1],_3p=E(_3l);return _3p[0]==0?[1,function(_3q){return new F(function(){return _3b(B(A(_3o,[_3q])),_3p);});}]:[1,function(_3r){return new F(function(){return _3b(B(A(_3o,[_3r])),new T(function(){return B(A(_3p[1],[_3r]));}));});}];}else{var _3s=E(_3l);return _3s[0]==0?E(_30):[1,function(_3t){return new F(function(){return _3b(_3n,new T(function(){return B(A(_3s[1],[_3t]));}));});}];}}},_3u=E(_3h);switch(_3u[0]){case 1:var _3v=E(_3i);if(_3v[0]==4){return [1,function(_3w){return [4,new T(function(){return B(_t(B(_31(B(A(_3u[1],[_3w])),_3w)),_3v[1]));})];}];}else{return new F(function(){return _3j(_);});}break;case 4:var _3x=_3u[1],_3y=E(_3i);switch(_3y[0]){case 0:return [1,function(_3z){return [4,new T(function(){return B(_t(_3x,new T(function(){return B(_31(_3y,_3z));})));})];}];case 1:return [1,function(_3A){return [4,new T(function(){return B(_t(_3x,new T(function(){return B(_31(B(A(_3y[1],[_3A])),_3A));})));})];}];default:return [4,new T(function(){return B(_t(_3x,_3y[1]));})];}break;default:return new F(function(){return _3j(_);});}}}}},_3B=E(_3c);switch(_3B[0]){case 0:var _3C=E(_3d);if(!_3C[0]){return [0,function(_3D){return new F(function(){return _3b(B(A(_3B[1],[_3D])),new T(function(){return B(A(_3C[1],[_3D]));}));});}];}else{return new F(function(){return _3e(_);});}break;case 3:return [3,_3B[1],new T(function(){return B(_3b(_3B[2],_3d));})];default:return new F(function(){return _3e(_);});}},_3E=[0,41],_3F=[1,_3E,_3],_3G=[0,40],_3H=[1,_3G,_3],_3I=function(_3J,_3K){while(1){var _3L=E(_3J);if(!_3L[0]){return E(_3K)[0]==0?true:false;}else{var _3M=E(_3K);if(!_3M[0]){return false;}else{if(E(_3L[1])[1]!=E(_3M[1])[1]){return false;}else{_3J=_3L[2];_3K=_3M[2];continue;}}}}},_3N=function(_3O,_3P){return E(_3O)[1]!=E(_3P)[1];},_3Q=function(_3R,_3S){return E(_3R)[1]==E(_3S)[1];},_3T=[0,_3Q,_3N],_3U=function(_3V,_3W){while(1){var _3X=E(_3V);if(!_3X[0]){return E(_3W)[0]==0?true:false;}else{var _3Y=E(_3W);if(!_3Y[0]){return false;}else{if(E(_3X[1])[1]!=E(_3Y[1])[1]){return false;}else{_3V=_3X[2];_3W=_3Y[2];continue;}}}}},_3Z=function(_40,_41){return !B(_3U(_40,_41))?true:false;},_42=[0,_3U,_3Z],_43=function(_44,_45){var _46=E(_44);switch(_46[0]){case 0:return [0,function(_47){return new F(function(){return _43(B(A(_46[1],[_47])),_45);});}];case 1:return [1,function(_48){return new F(function(){return _43(B(A(_46[1],[_48])),_45);});}];case 2:return [2];case 3:return new F(function(){return _3b(B(A(_45,[_46[1]])),new T(function(){return B(_43(_46[2],_45));}));});break;default:var _49=function(_4a){var _4b=E(_4a);if(!_4b[0]){return [0];}else{var _4c=E(_4b[1]);return new F(function(){return _t(B(_31(B(A(_45,[_4c[1]])),_4c[2])),new T(function(){return B(_49(_4b[2]));}));});}},_4d=B(_49(_46[1]));return _4d[0]==0?[2]:[4,_4d];}},_4e=function(_4f,_4g){var _4h=E(_4f);if(!_4h){return new F(function(){return A(_4g,[_1Z]);});}else{return [0,function(_4i){return E(new T(function(){return B(_4e(_4h-1|0,_4g));}));}];}},_4j=function(_4k,_4l,_4m){return function(_4n){return new F(function(){return A(function(_4o,_4p,_4q){while(1){var _4r=(function(_4s,_4t,_4u){var _4v=E(_4s);switch(_4v[0]){case 0:var _4w=E(_4t);if(!_4w[0]){return E(_4l);}else{_4o=B(A(_4v[1],[_4w[1]]));_4p=_4w[2];var _4x=_4u+1|0;_4q=_4x;return null;}break;case 1:var _4y=B(A(_4v[1],[_4t])),_4z=_4t,_4x=_4u;_4o=_4y;_4p=_4z;_4q=_4x;return null;case 2:return E(_4l);case 3:return function(_4A){return new F(function(){return _4e(_4u,function(_4B){return E(new T(function(){return B(_43(_4v,_4A));}));});});};default:return function(_4C){return new F(function(){return _43(_4v,_4C);});};}})(_4o,_4p,_4q);if(_4r!=null){return _4r;}}},[new T(function(){return B(A(_4k,[_24]));}),_4n,0,_4m]);});};},_4D=function(_4E){return new F(function(){return A(_4E,[_3]);});},_4F=function(_4G,_4H){var _4I=function(_4J){var _4K=E(_4J);if(!_4K[0]){return E(_4D);}else{var _4L=_4K[1];return !B(A(_4G,[_4L]))?E(_4D):function(_4M){return [0,function(_4N){return E(new T(function(){return B(A(new T(function(){return B(_4I(_4K[2]));}),[function(_4O){return new F(function(){return A(_4M,[[1,_4L,_4O]]);});}]));}));}];};}};return function(_4P){return new F(function(){return A(_4I,[_4P,_4H]);});};},_4Q=[6],_4R=function(_4S){return E(_4S);},_4T=new T(function(){return B(unCStr("valDig: Bad base"));}),_4U=new T(function(){return B(err(_4T));}),_4V=function(_4W,_4X){var _4Y=function(_4Z,_50){var _51=E(_4Z);if(!_51[0]){return function(_52){return new F(function(){return A(_52,[new T(function(){return B(A(_50,[_3]));})]);});};}else{var _53=E(_51[1])[1],_54=function(_55){return function(_56){return [0,function(_57){return E(new T(function(){return B(A(new T(function(){return B(_4Y(_51[2],function(_58){return new F(function(){return A(_50,[[1,_55,_58]]);});}));}),[_56]));}));}];};};switch(E(E(_4W)[1])){case 8:if(48>_53){return function(_59){return new F(function(){return A(_59,[new T(function(){return B(A(_50,[_3]));})]);});};}else{if(_53>55){return function(_5a){return new F(function(){return A(_5a,[new T(function(){return B(A(_50,[_3]));})]);});};}else{return new F(function(){return _54([0,_53-48|0]);});}}break;case 10:if(48>_53){return function(_5b){return new F(function(){return A(_5b,[new T(function(){return B(A(_50,[_3]));})]);});};}else{if(_53>57){return function(_5c){return new F(function(){return A(_5c,[new T(function(){return B(A(_50,[_3]));})]);});};}else{return new F(function(){return _54([0,_53-48|0]);});}}break;case 16:if(48>_53){if(97>_53){if(65>_53){return function(_5d){return new F(function(){return A(_5d,[new T(function(){return B(A(_50,[_3]));})]);});};}else{if(_53>70){return function(_5e){return new F(function(){return A(_5e,[new T(function(){return B(A(_50,[_3]));})]);});};}else{return new F(function(){return _54([0,(_53-65|0)+10|0]);});}}}else{if(_53>102){if(65>_53){return function(_5f){return new F(function(){return A(_5f,[new T(function(){return B(A(_50,[_3]));})]);});};}else{if(_53>70){return function(_5g){return new F(function(){return A(_5g,[new T(function(){return B(A(_50,[_3]));})]);});};}else{return new F(function(){return _54([0,(_53-65|0)+10|0]);});}}}else{return new F(function(){return _54([0,(_53-97|0)+10|0]);});}}}else{if(_53>57){if(97>_53){if(65>_53){return function(_5h){return new F(function(){return A(_5h,[new T(function(){return B(A(_50,[_3]));})]);});};}else{if(_53>70){return function(_5i){return new F(function(){return A(_5i,[new T(function(){return B(A(_50,[_3]));})]);});};}else{return new F(function(){return _54([0,(_53-65|0)+10|0]);});}}}else{if(_53>102){if(65>_53){return function(_5j){return new F(function(){return A(_5j,[new T(function(){return B(A(_50,[_3]));})]);});};}else{if(_53>70){return function(_5k){return new F(function(){return A(_5k,[new T(function(){return B(A(_50,[_3]));})]);});};}else{return new F(function(){return _54([0,(_53-65|0)+10|0]);});}}}else{return new F(function(){return _54([0,(_53-97|0)+10|0]);});}}}else{return new F(function(){return _54([0,_53-48|0]);});}}break;default:return E(_4U);}}};return function(_5l){return new F(function(){return A(_4Y,[_5l,_4R,function(_5m){var _5n=E(_5m);return _5n[0]==0?[2]:B(A(_4X,[_5n]));}]);});};},_5o=[0,10],_5p=[0,1],_5q=[0,2147483647],_5r=function(_5s,_5t){while(1){var _5u=E(_5s);if(!_5u[0]){var _5v=_5u[1],_5w=E(_5t);if(!_5w[0]){var _5x=_5w[1],_5y=addC(_5v,_5x);if(!E(_5y[2])){return [0,_5y[1]];}else{_5s=[1,I_fromInt(_5v)];_5t=[1,I_fromInt(_5x)];continue;}}else{_5s=[1,I_fromInt(_5v)];_5t=_5w;continue;}}else{var _5z=E(_5t);if(!_5z[0]){_5s=_5u;_5t=[1,I_fromInt(_5z[1])];continue;}else{return [1,I_add(_5u[1],_5z[1])];}}}},_5A=new T(function(){return B(_5r(_5q,_5p));}),_5B=function(_5C){var _5D=E(_5C);if(!_5D[0]){var _5E=E(_5D[1]);return _5E==(-2147483648)?E(_5A):[0, -_5E];}else{return [1,I_negate(_5D[1])];}},_5F=[0,10],_5G=[0,0],_5H=function(_5I){return [0,_5I];},_5J=function(_5K,_5L){while(1){var _5M=E(_5K);if(!_5M[0]){var _5N=_5M[1],_5O=E(_5L);if(!_5O[0]){var _5P=_5O[1];if(!(imul(_5N,_5P)|0)){return [0,imul(_5N,_5P)|0];}else{_5K=[1,I_fromInt(_5N)];_5L=[1,I_fromInt(_5P)];continue;}}else{_5K=[1,I_fromInt(_5N)];_5L=_5O;continue;}}else{var _5Q=E(_5L);if(!_5Q[0]){_5K=_5M;_5L=[1,I_fromInt(_5Q[1])];continue;}else{return [1,I_mul(_5M[1],_5Q[1])];}}}},_5R=function(_5S,_5T,_5U){while(1){var _5V=E(_5U);if(!_5V[0]){return E(_5T);}else{var _5W=B(_5r(B(_5J(_5T,_5S)),B(_5H(E(_5V[1])[1]))));_5U=_5V[2];_5T=_5W;continue;}}},_5X=function(_5Y){var _5Z=new T(function(){return B(_3b(B(_3b([0,function(_60){return E(E(_60)[1])==45?[1,B(_4V(_5o,function(_61){return new F(function(){return A(_5Y,[[1,new T(function(){return B(_5B(B(_5R(_5F,_5G,_61))));})]]);});}))]:[2];}],[0,function(_62){return E(E(_62)[1])==43?[1,B(_4V(_5o,function(_63){return new F(function(){return A(_5Y,[[1,new T(function(){return B(_5R(_5F,_5G,_63));})]]);});}))]:[2];}])),new T(function(){return [1,B(_4V(_5o,function(_64){return new F(function(){return A(_5Y,[[1,new T(function(){return B(_5R(_5F,_5G,_64));})]]);});}))];})));});return new F(function(){return _3b([0,function(_65){return E(E(_65)[1])==101?E(_5Z):[2];}],[0,function(_66){return E(E(_66)[1])==69?E(_5Z):[2];}]);});},_67=function(_68){return new F(function(){return A(_68,[_1D]);});},_69=function(_6a){return new F(function(){return A(_6a,[_1D]);});},_6b=function(_6c){return function(_6d){return E(E(_6d)[1])==46?[1,B(_4V(_5o,function(_6e){return new F(function(){return A(_6c,[[1,_6e]]);});}))]:[2];};},_6f=function(_6g){return [0,B(_6b(_6g))];},_6h=function(_6i){return new F(function(){return _4V(_5o,function(_6j){return [1,B(_4j(_6f,_67,function(_6k){return [1,B(_4j(_5X,_69,function(_6l){return new F(function(){return A(_6i,[[5,[1,_6j,_6k,_6l]]]);});}))];}))];});});},_6m=function(_6n){return [1,B(_6h(_6n))];},_6o=function(_6p){return E(E(_6p)[1]);},_6q=function(_6r,_6s,_6t){while(1){var _6u=E(_6t);if(!_6u[0]){return false;}else{if(!B(A(_6o,[_6r,_6s,_6u[1]]))){_6t=_6u[2];continue;}else{return true;}}}},_6v=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_6w=function(_6x){return new F(function(){return _6q(_3T,_6x,_6v);});},_6y=[0,8],_6z=[0,16],_6A=function(_6B){var _6C=function(_6D){return new F(function(){return A(_6B,[[5,[0,_6y,_6D]]]);});},_6E=function(_6F){return new F(function(){return A(_6B,[[5,[0,_6z,_6F]]]);});};return function(_6G){return E(E(_6G)[1])==48?E([0,function(_6H){switch(E(E(_6H)[1])){case 79:return [1,B(_4V(_6y,_6C))];case 88:return [1,B(_4V(_6z,_6E))];case 111:return [1,B(_4V(_6y,_6C))];case 120:return [1,B(_4V(_6z,_6E))];default:return [2];}}]):[2];};},_6I=function(_6J){return [0,B(_6A(_6J))];},_6K=false,_6L=true,_6M=function(_6N){var _6O=new T(function(){return B(A(_6N,[_6y]));}),_6P=new T(function(){return B(A(_6N,[_6z]));});return function(_6Q){switch(E(E(_6Q)[1])){case 79:return E(_6O);case 88:return E(_6P);case 111:return E(_6O);case 120:return E(_6P);default:return [2];}};},_6R=function(_6S){return [0,B(_6M(_6S))];},_6T=[0,92],_6U=function(_6V){return new F(function(){return A(_6V,[_5o]);});},_6W=function(_6X){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_1T(9,_6X,_3));}))));});},_6Y=function(_6Z){var _70=E(_6Z);return _70[0]==0?E(_70[1]):I_toInt(_70[1]);},_71=function(_72,_73){var _74=E(_72);if(!_74[0]){var _75=_74[1],_76=E(_73);return _76[0]==0?_75<=_76[1]:I_compareInt(_76[1],_75)>=0;}else{var _77=_74[1],_78=E(_73);return _78[0]==0?I_compareInt(_77,_78[1])<=0:I_compare(_77,_78[1])<=0;}},_79=function(_7a){return [2];},_7b=function(_7c){var _7d=E(_7c);if(!_7d[0]){return E(_79);}else{var _7e=_7d[1],_7f=E(_7d[2]);return _7f[0]==0?E(_7e):function(_7g){return new F(function(){return _3b(B(A(_7e,[_7g])),new T(function(){return B(A(new T(function(){return B(_7b(_7f));}),[_7g]));}));});};}},_7h=function(_7i){return [2];},_7j=function(_7k,_7l){var _7m=function(_7n,_7o){var _7p=E(_7n);if(!_7p[0]){return function(_7q){return new F(function(){return A(_7q,[_7k]);});};}else{var _7r=E(_7o);return _7r[0]==0?E(_7h):E(_7p[1])[1]!=E(_7r[1])[1]?E(_7h):function(_7s){return [0,function(_7t){return E(new T(function(){return B(A(new T(function(){return B(_7m(_7p[2],_7r[2]));}),[_7s]));}));}];};}};return function(_7u){return new F(function(){return A(_7m,[_7k,_7u,_7l]);});};},_7v=new T(function(){return B(unCStr("SOH"));}),_7w=[0,1],_7x=function(_7y){return [1,B(_7j(_7v,function(_7z){return E(new T(function(){return B(A(_7y,[_7w]));}));}))];},_7A=new T(function(){return B(unCStr("SO"));}),_7B=[0,14],_7C=function(_7D){return [1,B(_7j(_7A,function(_7E){return E(new T(function(){return B(A(_7D,[_7B]));}));}))];},_7F=function(_7G){return [1,B(_4j(_7x,_7C,_7G))];},_7H=new T(function(){return B(unCStr("NUL"));}),_7I=[0,0],_7J=function(_7K){return [1,B(_7j(_7H,function(_7L){return E(new T(function(){return B(A(_7K,[_7I]));}));}))];},_7M=new T(function(){return B(unCStr("STX"));}),_7N=[0,2],_7O=function(_7P){return [1,B(_7j(_7M,function(_7Q){return E(new T(function(){return B(A(_7P,[_7N]));}));}))];},_7R=new T(function(){return B(unCStr("ETX"));}),_7S=[0,3],_7T=function(_7U){return [1,B(_7j(_7R,function(_7V){return E(new T(function(){return B(A(_7U,[_7S]));}));}))];},_7W=new T(function(){return B(unCStr("EOT"));}),_7X=[0,4],_7Y=function(_7Z){return [1,B(_7j(_7W,function(_80){return E(new T(function(){return B(A(_7Z,[_7X]));}));}))];},_81=new T(function(){return B(unCStr("ENQ"));}),_82=[0,5],_83=function(_84){return [1,B(_7j(_81,function(_85){return E(new T(function(){return B(A(_84,[_82]));}));}))];},_86=new T(function(){return B(unCStr("ACK"));}),_87=[0,6],_88=function(_89){return [1,B(_7j(_86,function(_8a){return E(new T(function(){return B(A(_89,[_87]));}));}))];},_8b=new T(function(){return B(unCStr("BEL"));}),_8c=[0,7],_8d=function(_8e){return [1,B(_7j(_8b,function(_8f){return E(new T(function(){return B(A(_8e,[_8c]));}));}))];},_8g=new T(function(){return B(unCStr("BS"));}),_8h=[0,8],_8i=function(_8j){return [1,B(_7j(_8g,function(_8k){return E(new T(function(){return B(A(_8j,[_8h]));}));}))];},_8l=new T(function(){return B(unCStr("HT"));}),_8m=[0,9],_8n=function(_8o){return [1,B(_7j(_8l,function(_8p){return E(new T(function(){return B(A(_8o,[_8m]));}));}))];},_8q=new T(function(){return B(unCStr("LF"));}),_8r=[0,10],_8s=function(_8t){return [1,B(_7j(_8q,function(_8u){return E(new T(function(){return B(A(_8t,[_8r]));}));}))];},_8v=new T(function(){return B(unCStr("VT"));}),_8w=[0,11],_8x=function(_8y){return [1,B(_7j(_8v,function(_8z){return E(new T(function(){return B(A(_8y,[_8w]));}));}))];},_8A=new T(function(){return B(unCStr("FF"));}),_8B=[0,12],_8C=function(_8D){return [1,B(_7j(_8A,function(_8E){return E(new T(function(){return B(A(_8D,[_8B]));}));}))];},_8F=new T(function(){return B(unCStr("CR"));}),_8G=[0,13],_8H=function(_8I){return [1,B(_7j(_8F,function(_8J){return E(new T(function(){return B(A(_8I,[_8G]));}));}))];},_8K=new T(function(){return B(unCStr("SI"));}),_8L=[0,15],_8M=function(_8N){return [1,B(_7j(_8K,function(_8O){return E(new T(function(){return B(A(_8N,[_8L]));}));}))];},_8P=new T(function(){return B(unCStr("DLE"));}),_8Q=[0,16],_8R=function(_8S){return [1,B(_7j(_8P,function(_8T){return E(new T(function(){return B(A(_8S,[_8Q]));}));}))];},_8U=new T(function(){return B(unCStr("DC1"));}),_8V=[0,17],_8W=function(_8X){return [1,B(_7j(_8U,function(_8Y){return E(new T(function(){return B(A(_8X,[_8V]));}));}))];},_8Z=new T(function(){return B(unCStr("DC2"));}),_90=[0,18],_91=function(_92){return [1,B(_7j(_8Z,function(_93){return E(new T(function(){return B(A(_92,[_90]));}));}))];},_94=new T(function(){return B(unCStr("DC3"));}),_95=[0,19],_96=function(_97){return [1,B(_7j(_94,function(_98){return E(new T(function(){return B(A(_97,[_95]));}));}))];},_99=new T(function(){return B(unCStr("DC4"));}),_9a=[0,20],_9b=function(_9c){return [1,B(_7j(_99,function(_9d){return E(new T(function(){return B(A(_9c,[_9a]));}));}))];},_9e=new T(function(){return B(unCStr("NAK"));}),_9f=[0,21],_9g=function(_9h){return [1,B(_7j(_9e,function(_9i){return E(new T(function(){return B(A(_9h,[_9f]));}));}))];},_9j=new T(function(){return B(unCStr("SYN"));}),_9k=[0,22],_9l=function(_9m){return [1,B(_7j(_9j,function(_9n){return E(new T(function(){return B(A(_9m,[_9k]));}));}))];},_9o=new T(function(){return B(unCStr("ETB"));}),_9p=[0,23],_9q=function(_9r){return [1,B(_7j(_9o,function(_9s){return E(new T(function(){return B(A(_9r,[_9p]));}));}))];},_9t=new T(function(){return B(unCStr("CAN"));}),_9u=[0,24],_9v=function(_9w){return [1,B(_7j(_9t,function(_9x){return E(new T(function(){return B(A(_9w,[_9u]));}));}))];},_9y=new T(function(){return B(unCStr("EM"));}),_9z=[0,25],_9A=function(_9B){return [1,B(_7j(_9y,function(_9C){return E(new T(function(){return B(A(_9B,[_9z]));}));}))];},_9D=new T(function(){return B(unCStr("SUB"));}),_9E=[0,26],_9F=function(_9G){return [1,B(_7j(_9D,function(_9H){return E(new T(function(){return B(A(_9G,[_9E]));}));}))];},_9I=new T(function(){return B(unCStr("ESC"));}),_9J=[0,27],_9K=function(_9L){return [1,B(_7j(_9I,function(_9M){return E(new T(function(){return B(A(_9L,[_9J]));}));}))];},_9N=new T(function(){return B(unCStr("FS"));}),_9O=[0,28],_9P=function(_9Q){return [1,B(_7j(_9N,function(_9R){return E(new T(function(){return B(A(_9Q,[_9O]));}));}))];},_9S=new T(function(){return B(unCStr("GS"));}),_9T=[0,29],_9U=function(_9V){return [1,B(_7j(_9S,function(_9W){return E(new T(function(){return B(A(_9V,[_9T]));}));}))];},_9X=new T(function(){return B(unCStr("RS"));}),_9Y=[0,30],_9Z=function(_a0){return [1,B(_7j(_9X,function(_a1){return E(new T(function(){return B(A(_a0,[_9Y]));}));}))];},_a2=new T(function(){return B(unCStr("US"));}),_a3=[0,31],_a4=function(_a5){return [1,B(_7j(_a2,function(_a6){return E(new T(function(){return B(A(_a5,[_a3]));}));}))];},_a7=new T(function(){return B(unCStr("SP"));}),_a8=[0,32],_a9=function(_aa){return [1,B(_7j(_a7,function(_ab){return E(new T(function(){return B(A(_aa,[_a8]));}));}))];},_ac=new T(function(){return B(unCStr("DEL"));}),_ad=[0,127],_ae=function(_af){return [1,B(_7j(_ac,function(_ag){return E(new T(function(){return B(A(_af,[_ad]));}));}))];},_ah=[1,_ae,_3],_ai=[1,_a9,_ah],_aj=[1,_a4,_ai],_ak=[1,_9Z,_aj],_al=[1,_9U,_ak],_am=[1,_9P,_al],_an=[1,_9K,_am],_ao=[1,_9F,_an],_ap=[1,_9A,_ao],_aq=[1,_9v,_ap],_ar=[1,_9q,_aq],_as=[1,_9l,_ar],_at=[1,_9g,_as],_au=[1,_9b,_at],_av=[1,_96,_au],_aw=[1,_91,_av],_ax=[1,_8W,_aw],_ay=[1,_8R,_ax],_az=[1,_8M,_ay],_aA=[1,_8H,_az],_aB=[1,_8C,_aA],_aC=[1,_8x,_aB],_aD=[1,_8s,_aC],_aE=[1,_8n,_aD],_aF=[1,_8i,_aE],_aG=[1,_8d,_aF],_aH=[1,_88,_aG],_aI=[1,_83,_aH],_aJ=[1,_7Y,_aI],_aK=[1,_7T,_aJ],_aL=[1,_7O,_aK],_aM=[1,_7J,_aL],_aN=[1,_7F,_aM],_aO=new T(function(){return B(_7b(_aN));}),_aP=[0,1114111],_aQ=[0,34],_aR=[0,39],_aS=function(_aT){var _aU=new T(function(){return B(A(_aT,[_8c]));}),_aV=new T(function(){return B(A(_aT,[_8h]));}),_aW=new T(function(){return B(A(_aT,[_8m]));}),_aX=new T(function(){return B(A(_aT,[_8r]));}),_aY=new T(function(){return B(A(_aT,[_8w]));}),_aZ=new T(function(){return B(A(_aT,[_8B]));}),_b0=new T(function(){return B(A(_aT,[_8G]));});return new F(function(){return _3b([0,function(_b1){switch(E(E(_b1)[1])){case 34:return E(new T(function(){return B(A(_aT,[_aQ]));}));case 39:return E(new T(function(){return B(A(_aT,[_aR]));}));case 92:return E(new T(function(){return B(A(_aT,[_6T]));}));case 97:return E(_aU);case 98:return E(_aV);case 102:return E(_aZ);case 110:return E(_aX);case 114:return E(_b0);case 116:return E(_aW);case 118:return E(_aY);default:return [2];}}],new T(function(){return B(_3b([1,B(_4j(_6R,_6U,function(_b2){return [1,B(_4V(_b2,function(_b3){var _b4=B(_5R(new T(function(){return B(_5H(E(_b2)[1]));}),_5G,_b3));return !B(_71(_b4,_aP))?[2]:B(A(_aT,[new T(function(){var _b5=B(_6Y(_b4));if(_b5>>>0>1114111){var _b6=B(_6W(_b5));}else{var _b6=[0,_b5];}var _b7=_b6,_b8=_b7,_b9=_b8;return _b9;})]));}))];}))],new T(function(){return B(_3b([0,function(_ba){return E(E(_ba)[1])==94?E([0,function(_bb){switch(E(E(_bb)[1])){case 64:return E(new T(function(){return B(A(_aT,[_7I]));}));case 65:return E(new T(function(){return B(A(_aT,[_7w]));}));case 66:return E(new T(function(){return B(A(_aT,[_7N]));}));case 67:return E(new T(function(){return B(A(_aT,[_7S]));}));case 68:return E(new T(function(){return B(A(_aT,[_7X]));}));case 69:return E(new T(function(){return B(A(_aT,[_82]));}));case 70:return E(new T(function(){return B(A(_aT,[_87]));}));case 71:return E(_aU);case 72:return E(_aV);case 73:return E(_aW);case 74:return E(_aX);case 75:return E(_aY);case 76:return E(_aZ);case 77:return E(_b0);case 78:return E(new T(function(){return B(A(_aT,[_7B]));}));case 79:return E(new T(function(){return B(A(_aT,[_8L]));}));case 80:return E(new T(function(){return B(A(_aT,[_8Q]));}));case 81:return E(new T(function(){return B(A(_aT,[_8V]));}));case 82:return E(new T(function(){return B(A(_aT,[_90]));}));case 83:return E(new T(function(){return B(A(_aT,[_95]));}));case 84:return E(new T(function(){return B(A(_aT,[_9a]));}));case 85:return E(new T(function(){return B(A(_aT,[_9f]));}));case 86:return E(new T(function(){return B(A(_aT,[_9k]));}));case 87:return E(new T(function(){return B(A(_aT,[_9p]));}));case 88:return E(new T(function(){return B(A(_aT,[_9u]));}));case 89:return E(new T(function(){return B(A(_aT,[_9z]));}));case 90:return E(new T(function(){return B(A(_aT,[_9E]));}));case 91:return E(new T(function(){return B(A(_aT,[_9J]));}));case 92:return E(new T(function(){return B(A(_aT,[_9O]));}));case 93:return E(new T(function(){return B(A(_aT,[_9T]));}));case 94:return E(new T(function(){return B(A(_aT,[_9Y]));}));case 95:return E(new T(function(){return B(A(_aT,[_a3]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_aO,[_aT]));})));})));}));});},_bc=function(_bd){return new F(function(){return A(_bd,[_1Z]);});},_be=function(_bf){var _bg=E(_bf);if(!_bg[0]){return E(_bc);}else{var _bh=_bg[2],_bi=E(E(_bg[1])[1]);switch(_bi){case 9:return function(_bj){return [0,function(_bk){return E(new T(function(){return B(A(new T(function(){return B(_be(_bh));}),[_bj]));}));}];};case 10:return function(_bl){return [0,function(_bm){return E(new T(function(){return B(A(new T(function(){return B(_be(_bh));}),[_bl]));}));}];};case 11:return function(_bn){return [0,function(_bo){return E(new T(function(){return B(A(new T(function(){return B(_be(_bh));}),[_bn]));}));}];};case 12:return function(_bp){return [0,function(_bq){return E(new T(function(){return B(A(new T(function(){return B(_be(_bh));}),[_bp]));}));}];};case 13:return function(_br){return [0,function(_bs){return E(new T(function(){return B(A(new T(function(){return B(_be(_bh));}),[_br]));}));}];};case 32:return function(_bt){return [0,function(_bu){return E(new T(function(){return B(A(new T(function(){return B(_be(_bh));}),[_bt]));}));}];};case 160:return function(_bv){return [0,function(_bw){return E(new T(function(){return B(A(new T(function(){return B(_be(_bh));}),[_bv]));}));}];};default:var _bx=u_iswspace(_bi),_by=_bx;return E(_by)==0?E(_bc):function(_bz){return [0,function(_bA){return E(new T(function(){return B(A(new T(function(){return B(_be(_bh));}),[_bz]));}));}];};}}},_bB=function(_bC){var _bD=new T(function(){return B(_bB(_bC));}),_bE=[1,function(_bF){return new F(function(){return A(_be,[_bF,function(_bG){return E([0,function(_bH){return E(E(_bH)[1])==92?E(_bD):[2];}]);}]);});}];return new F(function(){return _3b([0,function(_bI){return E(E(_bI)[1])==92?E([0,function(_bJ){var _bK=E(E(_bJ)[1]);switch(_bK){case 9:return E(_bE);case 10:return E(_bE);case 11:return E(_bE);case 12:return E(_bE);case 13:return E(_bE);case 32:return E(_bE);case 38:return E(_bD);case 160:return E(_bE);default:var _bL=u_iswspace(_bK),_bM=_bL;return E(_bM)==0?[2]:E(_bE);}}]):[2];}],[0,function(_bN){var _bO=E(_bN);return E(_bO[1])==92?E(new T(function(){return B(_aS(function(_bP){return new F(function(){return A(_bC,[[0,_bP,_6L]]);});}));})):B(A(_bC,[[0,_bO,_6K]]));}]);});},_bQ=function(_bR,_bS){return new F(function(){return _bB(function(_bT){var _bU=E(_bT),_bV=E(_bU[1]);if(E(_bV[1])==34){if(!E(_bU[2])){return E(new T(function(){return B(A(_bS,[[1,new T(function(){return B(A(_bR,[_3]));})]]));}));}else{return new F(function(){return _bQ(function(_bW){return new F(function(){return A(_bR,[[1,_bV,_bW]]);});},_bS);});}}else{return new F(function(){return _bQ(function(_bX){return new F(function(){return A(_bR,[[1,_bV,_bX]]);});},_bS);});}});});},_bY=new T(function(){return B(unCStr("_\'"));}),_bZ=function(_c0){var _c1=u_iswalnum(_c0),_c2=_c1;return E(_c2)==0?B(_6q(_3T,[0,_c0],_bY)):true;},_c3=function(_c4){return new F(function(){return _bZ(E(_c4)[1]);});},_c5=new T(function(){return B(unCStr(",;()[]{}`"));}),_c6=new T(function(){return B(unCStr(".."));}),_c7=new T(function(){return B(unCStr("::"));}),_c8=new T(function(){return B(unCStr("->"));}),_c9=[0,64],_ca=[1,_c9,_3],_cb=[0,126],_cc=[1,_cb,_3],_cd=new T(function(){return B(unCStr("=>"));}),_ce=[1,_cd,_3],_cf=[1,_cc,_ce],_cg=[1,_ca,_cf],_ch=[1,_c8,_cg],_ci=new T(function(){return B(unCStr("<-"));}),_cj=[1,_ci,_ch],_ck=[0,124],_cl=[1,_ck,_3],_cm=[1,_cl,_cj],_cn=[1,_6T,_3],_co=[1,_cn,_cm],_cp=[0,61],_cq=[1,_cp,_3],_cr=[1,_cq,_co],_cs=[1,_c7,_cr],_ct=[1,_c6,_cs],_cu=function(_cv){return new F(function(){return _3b([1,function(_cw){return E(_cw)[0]==0?E(new T(function(){return B(A(_cv,[_4Q]));})):[2];}],new T(function(){return B(_3b([0,function(_cx){return E(E(_cx)[1])==39?E([0,function(_cy){var _cz=E(_cy);switch(E(_cz[1])){case 39:return [2];case 92:return E(new T(function(){return B(_aS(function(_cA){return [0,function(_cB){return E(E(_cB)[1])==39?E(new T(function(){return B(A(_cv,[[0,_cA]]));})):[2];}];}));}));default:return [0,function(_cC){return E(E(_cC)[1])==39?E(new T(function(){return B(A(_cv,[[0,_cz]]));})):[2];}];}}]):[2];}],new T(function(){return B(_3b([0,function(_cD){return E(E(_cD)[1])==34?E(new T(function(){return B(_bQ(_4R,_cv));})):[2];}],new T(function(){return B(_3b([0,function(_cE){return !B(_6q(_3T,_cE,_c5))?[2]:B(A(_cv,[[2,[1,_cE,_3]]]));}],new T(function(){return B(_3b([0,function(_cF){return !B(_6q(_3T,_cF,_6v))?[2]:[1,B(_4F(_6w,function(_cG){var _cH=[1,_cF,_cG];return !B(_6q(_42,_cH,_ct))?B(A(_cv,[[4,_cH]])):B(A(_cv,[[2,_cH]]));}))];}],new T(function(){return B(_3b([0,function(_cI){var _cJ=E(_cI),_cK=_cJ[1],_cL=u_iswalpha(_cK),_cM=_cL;return E(_cM)==0?E(_cK)==95?[1,B(_4F(_c3,function(_cN){return new F(function(){return A(_cv,[[3,[1,_cJ,_cN]]]);});}))]:[2]:[1,B(_4F(_c3,function(_cO){return new F(function(){return A(_cv,[[3,[1,_cJ,_cO]]]);});}))];}],new T(function(){return [1,B(_4j(_6I,_6m,_cv))];})));})));})));})));})));}));});},_cP=[0,0],_cQ=function(_cR,_cS){return function(_cT){return new F(function(){return A(_be,[_cT,function(_cU){return E(new T(function(){return B(_cu(function(_cV){var _cW=E(_cV);return _cW[0]==2?!B(_3I(_cW[1],_3H))?[2]:E(new T(function(){return B(A(_cR,[_cP,function(_cX){return [1,function(_cY){return new F(function(){return A(_be,[_cY,function(_cZ){return E(new T(function(){return B(_cu(function(_d0){var _d1=E(_d0);return _d1[0]==2?!B(_3I(_d1[1],_3F))?[2]:E(new T(function(){return B(A(_cS,[_cX]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_d2=function(_d3,_d4,_d5){var _d6=function(_d7,_d8){return new F(function(){return _3b([1,function(_d9){return new F(function(){return A(_be,[_d9,function(_da){return E(new T(function(){return B(_cu(function(_db){var _dc=E(_db);if(_dc[0]==4){var _dd=E(_dc[1]);if(!_dd[0]){return new F(function(){return A(_d3,[_dc,_d7,_d8]);});}else{return E(E(_dd[1])[1])==45?E(_dd[2])[0]==0?E([1,function(_de){return new F(function(){return A(_be,[_de,function(_df){return E(new T(function(){return B(_cu(function(_dg){return new F(function(){return A(_d3,[_dg,_d7,function(_dh){return new F(function(){return A(_d8,[new T(function(){return [0, -E(_dh)[1]];})]);});}]);});}));}));}]);});}]):B(A(_d3,[_dc,_d7,_d8])):B(A(_d3,[_dc,_d7,_d8]));}}else{return new F(function(){return A(_d3,[_dc,_d7,_d8]);});}}));}));}]);});}],new T(function(){return [1,B(_cQ(_d6,_d8))];}));});};return new F(function(){return _d6(_d4,_d5);});},_di=function(_dj,_dk){return [2];},_dl=function(_dm){var _dn=E(_dm);return _dn[0]==0?[1,new T(function(){return B(_5R(new T(function(){return B(_5H(E(_dn[1])[1]));}),_5G,_dn[2]));})]:E(_dn[2])[0]==0?E(_dn[3])[0]==0?[1,new T(function(){return B(_5R(_5F,_5G,_dn[1]));})]:[0]:[0];},_do=function(_dp){var _dq=E(_dp);if(_dq[0]==5){var _dr=B(_dl(_dq[1]));return _dr[0]==0?E(_di):function(_ds,_dt){return new F(function(){return A(_dt,[new T(function(){return [0,B(_6Y(_dr[1]))];})]);});};}else{return E(_di);}},_du=new T(function(){return B(_d2(_do,_cP,_24));}),_dv=new T(function(){return B(unCStr("do not use readS_to_P in gather!"));}),_dw=new T(function(){return B(err(_dv));}),_dx=function(_dy,_dz){var _dA=E(_dz);switch(_dA[0]){case 0:return [0,function(_dB){return new F(function(){return _dx(function(_dC){return new F(function(){return A(_dy,[[1,_dB,_dC]]);});},B(A(_dA[1],[_dB])));});}];case 1:return [1,function(_dD){return new F(function(){return _dx(_dy,B(A(_dA[1],[_dD])));});}];case 2:return [2];case 3:return new F(function(){return _3b(B(A(_dA[1],[new T(function(){return B(A(_dy,[_3]));})])),new T(function(){return B(_dx(_dy,_dA[2]));}));});break;default:return E(_dw);}},_dE=[3,_24,_23],_dF=function(_dG){return E(_dE);},_dH=new T(function(){return B(_cu(_dF));}),_dI=new T(function(){return B(_dx(_4R,_dH));}),_dJ=function(_dK){return E(_dI);},_dL=function(_dM){return new F(function(){return A(_be,[_dM,_dJ]);});},_dN=[1,_dL],_dO=function(_dP){var _dQ=E(_dP);if(!_dQ[0]){return [0];}else{var _dR=E(_dQ[1]),_dS=function(_dT){while(1){var _dU=(function(_dV){var _dW=E(_dV);if(!_dW[0]){return E(new T(function(){return B(_dO(_dQ[2]));}));}else{var _dX=_dW[2],_dY=E(_dW[1]);if(!E(_dY[1])[0]){if(!E(_dY[2])[0]){return [1,_dR[1],new T(function(){return B(_dS(_dX));})];}else{_dT=_dX;return null;}}else{_dT=_dX;return null;}}})(_dT);if(_dU!=null){return _dU;}}};return new F(function(){return _dS(B(_31(_dN,_dR[2])));});}},_dZ=function(_e0){var _e1=B(_dO(B(_31(_du,_e0))));return _e1[0]==0?[0]:E(_e1[2])[0]==0?[1,_e1[1]]:[0];},_e2=function(_e3){return new F(function(){return _dZ(new T(function(){var _e4=js_parseChangeEvent(E(_e3)[1]),_e5=_e4;return fromJSStr(E(_e5)[1]);}));});},_e6=function(_e7,_e8){return new F(function(){return _e2(_e8);});},_e9=[0,_e6,_22],_ea=[1,_e9,_3],_eb=new T(function(){return [0,"value"];}),_ec=function(_ed){return [0,new T(function(){return B(_t(_3,[1,[1,_21,[1,[0,_eb,new T(function(){var _ee=E(_ed);if(!_ee[0]){var _ef=[1,E(_20)[1]];}else{var _ef=[1,toJSStr(B(_1T(0,E(_ee[1])[1],_3)))];}return _ef;})],_3],_ea],_3]));}),_ed,_1Z];},_eg=new T(function(){return [0,"div"];}),_eh=function(_ei,_ej){var _ek=js_parseChangeEvent(E(_ej)[1]),_el=_ek;return E(_el);},_em=[0,_eh,_22],_en=[1,_em,_3],_eo=function(_ep,_eq){return E(_eq);},_er=function(_es,_et){return E(_et);},_eu=[0,_er,_eo],_ev=function(_ew,_ex){return E(_ew);},_ey=function(_ez,_eA){return new F(function(){return A(_ez,[_eA]);});},_eB=[0,_ey,_ev],_eC=function(_eD){return E(_eD);},_eE=function(_eF,_eG,_eH){return [0,function(_eI,_eJ){return new F(function(){return A(_eF,[_eB,function(_eK){return E(new T(function(){return B(A(_eG,[new T(function(){return B(A(_eF,[_eu,_eC,_eI]));}),_eJ]));}));},_eI]);});},_eH];},_eL=function(_eM,_eN){var _eO=E(_eN),_eP=B(_eE(_eM,_eO[1],_eO[2]));return [0,_eP[1],_eP[2]];},_eQ=function(_eR,_eS){var _eT=E(_eS);return _eT[0]==0?[0]:[1,new T(function(){return B(A(_eR,[_eT[1]]));}),new T(function(){return B(_eQ(_eR,_eT[2]));})];},_eU=function(_eV,_eW){var _eX=E(_eW);switch(_eX[0]){case 0:return [0,_eX[1],_eX[2],new T(function(){return B(_eQ(function(_eY){return new F(function(){return _eL(_eV,_eY);});},_eX[3]));}),new T(function(){return B(_eQ(function(_eY){return new F(function(){return _eU(_eV,_eY);});},_eX[4]));})];case 1:return [1,_eX[1],_eX[2],new T(function(){return B(_eQ(function(_eY){return new F(function(){return _eL(_eV,_eY);});},_eX[3]));})];default:return [2,_eX[1]];}},_eZ=function(_f0){return E(E(_f0)[1]);},_f1=function(_f2,_f3,_f4){var _f5=E(_f4);return new F(function(){return A(_eZ,[_f2,function(_f6){return [0,_f5[1],_f6];},new T(function(){return B(A(_f3,[_f5[2]]));})]);});},_f7=function(_f8){return new F(function(){return _eU(_f1,_f8);});},_f9=function(_fa,_fb,_fc){var _fd=E(_fc);return new F(function(){return A(_eZ,[_fa,function(_fe){return [0,_fe,_fd[2]];},new T(function(){return B(A(_fb,[_fd[1]]));})]);});},_ff=function(_f8){return new F(function(){return _eU(_f9,_f8);});},_fg=function(_fh){var _fi=new T(function(){var _fj=B(_ec(new T(function(){return E(E(_fh)[1]);})));return [0,_fj[1],_fj[2],_fj[3]];}),_fk=new T(function(){return [0,new T(function(){return E(E(_fi)[2]);}),E(_fh)[2]];}),_fl=new T(function(){return E(E(_fk)[2]);});return [0,[1,[0,_eg,_3,_3,new T(function(){return B(_t(B(_eQ(_ff,[1,[0,_eg,_3,_3,new T(function(){return E(E(_fi)[1]);})],_3])),new T(function(){return B(_eQ(_f7,[1,[0,_eg,_3,_3,new T(function(){return B(_t(_3,[1,[1,_21,[1,[0,_eb,new T(function(){return [1,E(_fl)[1]];})],_3],_en],_3]));})],_3]));})));})],_3],new T(function(){return [0,E(_fk)[1],_fl];}),_1Z];},_fm=function(_fn){var _fo=B(_fg(_fn));return [0,_fo[1],_fo[2],_fo[3]];},_fp=[0,1],_fq=[1,_fp],_fr=new T(function(){return [0,"foo"];}),_fs=[0,_fq,_fr],_ft=new T(function(){return B(_2Y("src/React.hs:(70,1)-(80,37)|function setField"));}),_fu=function(_fv,_fw,_fx,_){var _fy=E(_fx);switch(_fy[0]){case 0:var _fz=js_set_field(E(_fv)[1],E(_fw)[1],_fy[1]);return _1Z;case 1:var _fA=js_set_field(E(_fv)[1],E(_fw)[1],_fy[1]);return _1Z;case 2:if(!E(_fy[1])){var _fB=js_set_field_False(E(_fv)[1],E(_fw)[1]);return _1Z;}else{var _fC=js_set_field_True(E(_fv)[1],E(_fw)[1]);return _1Z;}break;case 3:return E(_ft);case 4:var _fD=js_empty_object(),_fE=_fD,_fF=B((function(_fG,_){while(1){var _fH=E(_fG);if(!_fH[0]){return _1Z;}else{var _fI=E(_fH[1]),_fJ=B(_fu([0,_fE],_fI[1],_fI[2],_)),_fK=_fJ;_fG=_fH[2];continue;}}})(_fy[1],_)),_fL=_fF,_fM=js_set_field(E(_fv)[1],E(_fw)[1],_fE);return _1Z;default:return _1Z;}},_fN=function(_fO,_fP,_fQ,_fR,_fS,_){var _fT=js_empty_object(),_fU=_fT,_fV=[0,_fU],_fW=B((function(_fX,_){while(1){var _fY=E(_fX);if(!_fY[0]){return _1Z;}else{var _fZ=E(_fY[1]),_g0=B(_fu(_fV,_fZ[1],_fZ[2],_)),_g1=_g0;_fX=_fY[2];continue;}}})(_fQ,_)),_g2=_fW,_g3=B((function(_g4,_){while(1){var _g5=E(_g4);if(!_g5[0]){return _1Z;}else{var _g6=_g5[2],_g7=E(_g5[1]),_g8=_g7[1];switch(E(_g7[2])){case 0:var _g9=js_set_onChange(_g8,_fU);_g4=_g6;continue;case 1:var _ga=js_set_onKeyDown(_g8,_fU);_g4=_g6;continue;case 2:var _gb=js_set_onKeyPress(_g8,_fU);_g4=_g6;continue;case 3:var _gc=js_set_onKeyUp(_g8,_fU);_g4=_g6;continue;case 4:var _gd=js_set_onClick(_g8,_fU);_g4=_g6;continue;case 5:var _ge=js_set_onDoubleClick(_g8,_fU);_g4=_g6;continue;case 6:var _gf=js_set_onMouseEnter(_g8,_fU);_g4=_g6;continue;default:var _gg=js_set_onMouseLeave(_g8,_fU);_g4=_g6;continue;}}}})(_fR,_)),_gh=_g3,_gi=js_empty(),_gj=_gi,_gk=B((function(_gl,_){while(1){var _gm=E(_gl);if(!_gm[0]){return _1Z;}else{var _gn=js_push(_gj,E(_gm[1])[1]);_gl=_gm[2];continue;}}})(_fS,_)),_go=_gk;return new F(function(){return A(_fO,[_fP,_fV,[0,_gj],_]);});},_gp=function(_gq,_gr,_gs,_){var _gt=js_React_DOM_parent(E(_gq)[1],E(_gr)[1],E(_gs)[1]),_gu=_gt;return [0,_gu];},_gv=function(_gw,_gx,_gy,_){return new F(function(){return _gp(_gw,_gx,_gy,_);});},_gz=function(_gA,_gB,_gC,_){var _gD=js_React_DOM_leaf(E(_gA)[1],E(_gB)[1]),_gE=_gD;return [0,_gE];},_gF=function(_gG,_gH,_gI){var _gJ=E(_gI);return [0,function(_gK){return new F(function(){return A(_gH,[new T(function(){return B(A(new T(function(){return B(A(_gJ[1],[_gG]));}),[_gK]));})]);});},_gJ[2]];},_gL=function(_gM,_gN,_gO,_){var _gP=E(_gO);switch(_gP[0]){case 0:var _gQ=function(_gR,_){var _gS=E(_gR);if(!_gS[0]){return _3;}else{var _gT=B(_gL(_gM,_gN,_gS[1],_)),_gU=_gT,_gV=B(_gQ(_gS[2],_)),_gW=_gV;return [1,_gU,_gW];}},_gX=B(_gQ(_gP[4],_)),_gY=_gX;return new F(function(){return _fN(_gv,_gP[1],_gP[2],new T(function(){return B(_eQ(function(_gZ){return new F(function(){return _gF(_gM,_gN,_gZ);});},_gP[3]));}),_gY,_);});break;case 1:return new F(function(){return _fN(_gz,_gP[1],_gP[2],new T(function(){return B(_eQ(function(_gZ){return new F(function(){return _gF(_gM,_gN,_gZ);});},_gP[3]));}),_3,_);});break;default:var _h0=js_id(toJSStr(E(_gP[1]))),_h1=_h0;return [0,_h1];}},_h2=function(_h3){var _h4=B(A(_h3,[_])),_h5=_h4;return E(_h5);},_h6=function(_h7){return new F(function(){return _h2(function(_){var _=0;return new F(function(){return eval(_h7);});});});},_h8=new T(function(){return B(_h6("(function(e,r){React.render(r,e);})"));}),_h9=new T(function(){return B(unCStr("Irrefutable pattern failed for pattern"));}),_ha=function(_hb){return new F(function(){return _2A([0,new T(function(){return B(_2P(_hb,_h9));})],_2x);});},_hc=new T(function(){return B(_ha("src/React.hs:98:5-51|(child : _, s\', ())"));}),_hd=function(_he,_hf,_hg,_){var _hh=B(A(_hg,[_he])),_hi=E(_hh[1]);if(!_hi[0]){return E(_hc);}else{var _hj=E(_hh[3]),_hk=B(_gL(_hh[2],function(_hl,_){return new F(function(){return _hd(_hl,_hf,_hg,_);});},_hi[1],_)),_hm=_hk,_hn=B(A(_h8,[E(E(_hf)[1]),E(E(_hm)[1]),_])),_ho=_hn;return _1Z;}},_hp=function(_){var _hq=jsFind("inject"),_hr=_hq,_hs=E(_hr);return _hs[0]==0?B(_1J(_1L,_)):B(_hd(_fs,_hs[1],_fm,_));},_ht=function(_){return new F(function(){return _hp(_);});};
var hasteMain = function() {B(A(_ht, [0]));};window.onload = hasteMain;