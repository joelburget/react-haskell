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
