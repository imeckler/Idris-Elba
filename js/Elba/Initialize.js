Elm = {Native : {}};
ElmRuntime = {};

Elm.fullscreen = function(module) {
    var style = document.createElement('style');
    style.type = 'text/css';
    style.innerHTML = "html,head,body { padding:0; margin:0; }" +
        "body { font-family: calibri, helvetica, arial, sans-serif; }";
    document.head.appendChild(style);
    var container = document.createElement('div');
    document.body.appendChild(container);
    return init(ElmRuntime.Display.FULLSCREEN, container, module);
};

Elm.domNode = function(container, module) {
    var tag = container.tagName;
    if (tag !== 'DIV') {
        throw new Error('Elm.node must be given a DIV, not a ' + tag + '.');
    } else if (container.hasChildNodes()) {
        throw new Error('Elm.node must be given an empty DIV. No children allowed!');
    }
    return init(ElmRuntime.Display.COMPONENT, container, module);
};

Elm.worker = function(module) {
    return init(ElmRuntime.Display.NONE, {}, module);
};

function init(display, container, module, moduleToReplace) {
  // defining state needed for an instance of the Elm RTS
  var inputs = [];

  function notify(id, v) {
      var timestep = Date.now();
      var changed = false;
      for (var i = inputs.length; i--; ) {
          // order is important here to avoid short-circuiting
          changed = inputs[i].recv(timestep, id, v) || changed;
      }
      return changed;
  }

  container.offsetX = 0;
  container.offsetY = 0;

  var listeners = [];
  function addListener(relevantInputs, domNode, eventName, func) {
      domNode.addEventListener(eventName, func);
      var listener = {
          relevantInputs: relevantInputs,
          domNode: domNode,
          eventName: eventName,
          func: func
      };
      listeners.push(listener);
  }

  // create the actual RTS. Any impure modules will attach themselves to this
  // object. This permits many Elm programs to be embedded per document.
  return {
      notify:notify,
      node:container,
      display:display,
      id:ElmRuntime.guid(),
      addListener:addListener,
      inputs:inputs
  };

  // Set up methods to communicate with Elm program from JS.
  function send(name, value) {
      if (typeof value === 'undefined') return function(v) { return send(name,v); };
      var e = document.createEvent('Event');
      e.initEvent(name + '_' + elm.id, true, true);
      e.value = value;
      document.dispatchEvent(e);
  }
  function recv(name, handler) {
      document.addEventListener(name + '_' + elm.id, handler);
  }

  recv('log', function(e) {console.log(e.value)});
  recv('title', function(e) {document.title = e.value});
  recv('redirect', function(e) {
    if (e.value.length > 0) { window.location = e.value; }
  });

  function swap(newModule) {
      removeListeners(listeners);
      var div = document.createElement('div');
      var newElm = init(display, div, newModule, elm);
      inputs = [];
      // elm.send = newElm.send;
      // elm.recv = newElm.recv;
      // elm.swap = newElm.swap;
      return newElm;
  }

  var Module = {};
  var reportAnyErrors = function() {};
  try {
      Module = module(elm);
  } catch(e) {
      var directions = "<br/>&nbsp; &nbsp; Open the developer console for more details."
      Module.main = Elm.Text(elm).text('<code>' + e.message + directions + '</code>');
      reportAnyErrors = function() { throw e; }
  }
  inputs = ElmRuntime.filterDeadInputs(inputs);
  filterListeners(inputs, listeners);
  if (display !== ElmRuntime.Display.NONE) {
      var graphicsNode = initGraphics(elm, Module);
  }
  if (typeof moduleToReplace !== 'undefined') {
      ElmRuntime.swap(moduleToReplace, elm);

      // rerender scene if graphics are enabled.
      if (typeof graphicsNode !== 'undefined') {
          graphicsNode.recv(0, true, 0);
      }
  }

  reportAnyErrors();
  return { send:send, recv:recv, swap:swap };
};

function filterListeners(inputs, listeners) {
    loop:
    for (var i = listeners.length; i--; ) {
        var listener = listeners[i];
        for (var j = inputs.length; j--; ) {
            if (listener.relevantInputs.indexOf(inputs[j].id) >= 0) {
                continue loop;
            }
        }
        listener.domNode.removeEventListener(listener.eventName, listener.func);
    }
}

function removeListeners(listeners) {
    for (var i = listeners.length; i--; ) {
        var listener = listeners[i];
        listener.domNode.removeEventListener(listener.eventName, listener.func);
    }
}

function initGraphics(elm, Module) {
  if (!('main' in Module))
      throw new Error("'main' is missing! What do I display?!");

  var signalGraph = Module.main;

  // make sure the signal graph is actually a signal & extract the visual model
  var Signal = Elm.Signal(elm);
  if (!('recv' in signalGraph)) {
      signalGraph = Signal.constant(signalGraph);
  }
  var currentScene = signalGraph.value;
  
 // Add the currentScene to the DOM
  var Render = ElmRuntime.use(ElmRuntime.Render.Element);
  elm.node.appendChild(Render.render(currentScene));
  
  // set up updates so that the DOM is adjusted as necessary.
  function domUpdate(newScene, currentScene) {
      ElmRuntime.draw(function(_) {
          Render.update(elm.node.firstChild, currentScene, newScene);
          if (elm.Native.Window) elm.Native.Window.resizeIfNeeded();
      });
      return newScene;
  }
  var renderer = Signal.foldp(domUpdate, currentScene, signalGraph);

  // must check for resize after 'renderer' is created so
  // that changes show up.
  if (elm.Native.Window) elm.Native.Window.resizeIfNeeded();

  return renderer;
}

(function(window){
  var elm = init(ElmRuntime.Display, document.body);

  Elm.Native.Utils(elm);
  Elm.Signal(elm);
  Elm.Native.Keyboard(elm);
  Elm.Native.Mouse(elm);

  var idrisUnit = null;
  var Elba = {
    newIORef :
      function(x){
        return [x];
        // return { contents : x };
      },

    readIORef :
      function(ref){
        return ref[0];
        // return ref.contents;
      },

    writeIORef :
      function(ref, x){
        ref[0] = x;
        // ref.contents = x;
      },

    iter:
      function(f, arr){
        var len = arr.length;
        for (var i = 0; i < len; ++i){
          f(arr[i]);
        }
      }
  };

  Elba.Mouse = {
    position : elm.Native.Mouse.position,
    clicks   : elm.Native.Mouse.clicks
  };

  Elba.Keyboard = {
    isDown : elm.Native.Keyboard.Elba.isDown,
    keysDown : elm.Native.Keyboard.Elba.keysDown,
    directions : elm.Native.Keyboard.Elba.directions,
    lastPressed : elm.Native.Keyboard.Elba.lastPressed
  };

  Elba.Signal = {
    constant : elm.Native.Signal.Uncurried.constant,

    map : function(idrFun, sig){
      return elm.Native.Signal.Uncurried.lift(wrapIdrisUncurried(idrFun), sig);
    },

    ap : function(fStream, xStream){
      return elm.Native.Signal.Uncurried.lift2(function(f, x){
        return wrapIdrisUncurried(f)(x);
      }, fStream, xStream);
    },

    dropIf : function(idrPred, init, sig){
      return elm.Native.Signal.Uncurried.dropIf(wrapIdrisUncurried(idrPred), init, sig);
    },

    keepIf : function(idrPred, init, sig){
      return elm.Native.Signal.Uncurried.dropIf(function(x){
        return ! (wrapIdrisUncurried(idrPred)(x));
      }, init, sig);
    },

    scan : function(f, z, sig) {
      return elm.Native.Signal.Uncurried.scan(wrapIdrisUncurried(f), z, sig);
    },

    bind : function(f, sig) {
      return elm.Native.Signal.Uncurried.bind(wrapIdrisUncurried(f), sig);
    },

    join : elm.Native.Signal.Uncurried.join,

    switcher : elm.Native.Signal.Uncurried.switcher,

    merge : elm.Native.Signal.merge,

    dropWhen : elm.Native.Signal.Uncurried.dropWhen,

    keepWhen : elm.Native.Signal.Uncurried.keepWhen,

    sampleOn : elm.Native.Signal.Uncurried.sampleOn,

    dropRepeats : function(idrEq, sig){
      return elm.Native.Signal.Uncurried.dropRepeatsEq(wrapIdrisUncurried(idrEq), sig);
    },
    
    sink : function(f, sig){
      return elm.Native.Signal.Uncurried.sink(wrapIdrisUncurried(f), sig);
    }
  }

  window.Elba = Elba;
})(window);
