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
