function js_set_onClick(cb, attrs) {
    attrs.onClick = function(e) {
        e.persist();
        cb(e);
    };
}
function js_set_onDoubleClick(cb, attrs) {
    attrs.onDoubleClick = function(e) {
        e.persist();
        cb(e);
    };
}
function js_set_onChange(cb, attrs) {
    attrs.onChange = function(e) {
        e.persist();
        cb(e);
    };
}
function js_set_onKeyUp(cb, attrs) {
    attrs.onKeyUp = function(e) {
        e.persist();
        cb(e);
    };
}
function js_set_onKeyPress(cb, attrs) {
    attrs.onKeyPress = function(e) {
        e.persist();
        cb(e);
    };
}
function js_set_onKeyDown(cb, attrs) {
    attrs.onKeyDown = function(e) {
        e.persist();
        cb(e);
    };
}
function js_set_onBlur(cb, attrs) {
    attrs.onBlur = function(e) {
        e.persist();
        cb(e);
    };
}
function js_set_onMouseEnter(cb, attrs) {
    attrs.onMouseEnter = function(e) {
        e.persist();
        cb(e);
    };
}
function js_set_onMouseLeave(cb, attrs) {
    attrs.onMouseLeave = function(e) {
        e.persist();
        cb(e);
    };
}

function js_React_DOM(tagName, attrs) {return React.DOM[tagName](attrs);}

function js_empty() {return [];}
function js_push(a,v) {a.push(v);}

// polyfill from http://www.paulirish.com/2011/requestanimationframe-for-smart-animating/
(function() {
    var lastTime = 0;
    var vendors = ['webkit', 'moz'];
    for(var x = 0; x < vendors.length && !window.requestAnimationFrame; ++x) {
        window.requestAnimationFrame = window[vendors[x]+'RequestAnimationFrame'];
        window.cancelAnimationFrame =
          window[vendors[x]+'CancelAnimationFrame'] || window[vendors[x]+'CancelRequestAnimationFrame'];
    }

    if (!window.requestAnimationFrame)
        window.requestAnimationFrame = function(callback, element) {
            var currTime = new Date().getTime();
            var timeToCall = Math.max(0, 16 - (currTime - lastTime));
            var id = window.setTimeout(function() { callback(currTime + timeToCall); },
              timeToCall);
            lastTime = currTime + timeToCall;
            return id;
        };

    if (!window.cancelAnimationFrame)
        window.cancelAnimationFrame = function(id) {
            clearTimeout(id);
        };
}());

function js_raf(cb) {
    return window.requestAnimationFrame(function(time) {
        B(A(cb,[[0,time],0]));
    });
}

function js_createClass(render, getInitialState, _) {
    return React.createClass({
        render: function() {
          // render :: a -> b -> IO ForeignNode
          // need either
          //   - something like runIO
          //   - render to not run in the IO monad
          //   - React to use continuation style passing
          return B(A(render, [[0, this], [0, this.state.hs], 0]))[1];
        },
        getInitialState: function() { return {hs: B(A(getInitialState, [[0, this], 0]))} }
    });
}

function js_bezier(x0, y0, x1, y1, x) {
    return BezierEasing(x0, y0, x1, y1)(x);
}

function js_render(e, r){
    React.render(React.createElement(e, null), r);
}

function js_cancelRaf(id) {
    window.cancelAnimationFrame(id);
}

function js_getState(inst) {
  return inst.state.hs;
}

function js_setState(inst, state) {
  inst.replaceState({hs: state});
}

function js_overState(inst, func) {
  inst.replaceState({hs: B(A(func, [[0, inst.state.hs], 0]))[1]});
}

function js_performance_now() {
  return window.performance.now();
}
