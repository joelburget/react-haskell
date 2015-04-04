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
        console.log("triggering", e);
        cb(e);
    };
    console.log("js_set_onKeyDown", attrs);
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
