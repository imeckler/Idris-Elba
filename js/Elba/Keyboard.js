
Elm.Native.Keyboard = function(elm) {
  'use strict';
  elm.Native = elm.Native || {};
  if (elm.Native.Keyboard) return elm.Native.Keyboard;

  // Duplicated from Native.Signal
  function send(node, timestep, changed) {
    var kids = node.kids;
    for (var i = kids.length; i--; ) {
      kids[i].recv(timestep, changed, node.id);
    }
  }

  var Signal = Elm.Signal(elm);
  var NList = Elm.Native.List(elm);
  var Utils = Elm.Native.Utils(elm);

  var downEvents = Signal.constant(0);
  var upEvents = Signal.constant(0);
  var blurEvents = Signal.constant(0);

  elm.addListener([downEvents.id], document, 'keydown', function down(e) {
    elm.notify(downEvents.id, e.keyCode);
  });

  elm.addListener([upEvents.id], document, 'keyup', function up(e) {
    elm.notify(upEvents.id, e.keyCode);
  });

  elm.addListener([blurEvents.id], document, 'blur', function blur(e) {
    elm.notify(blurEvents.id, NList.Nil);
  });

  function arrMember(x, arr) {
    var len = arr.length;
    for (var i = 0; i < len; ++i){
      if (x === arr[i]) {
        return true;
      }
    }
    return false;
  }

  function KeyMergeArr(down, up, blur) {
    var args = [down,up,blur];
    this.id = Utils.guid();
    // Ignore starting values here
    this.value = [];
    this.kids = [];
    
    var n = args.length;
    var count = 0;
    var isChanged = false;

    this.recv = function(timestep, changed, parentID) {
      ++count;
      if (changed) { 
        // We know this a change must only be one of the following cases
        if (parentID === down.id && !(arrMember(down.value, this.value))) {
          isChanged = true;
          var newValue = this.value.slice(0);
          newValue.push(down.value);
          this.value = newValue;
        } 
        if (parentID === up.id) {
          isChanged = true;
          var notEq = function(kc) { return kc !== up.value };
          this.value = this.value.filter(notEq);
        } 
        if (parentID === blur.id) {
          isChanged = true;
          this.value = [];
        }
      }
      if (count == n) {
        send(this, timestep, isChanged);
        isChanged = false;
        count = 0;
      }
    };

    for (var i = n; i--; ) { args[i].kids.push(this); }
  }

  function KeyMerge(down, up, blur) {
    var args = [down,up,blur];
    this.id = Utils.guid();
    // Ignore starting values here
    this.value = NList.Nil
    this.kids = [];
    
    var n = args.length;
    var count = 0;
    var isChanged = false;

    this.recv = function(timestep, changed, parentID) {
      ++count;
      if (changed) { 
        // We know this a change must only be one of the following cases
        if (parentID === down.id && !(NList.member(down.value)(this.value))) {
          isChanged = true;
          this.value = NList.Cons(down.value, this.value); 
        } 
        if (parentID === up.id) {
          isChanged = true;
          var notEq = function(kc) { return kc !== up.value };
          this.value = NList.filter(notEq)(this.value);
        } 
        if (parentID === blur.id) {
          isChanged = true;
          this.value = NList.Nil;
        }
      }
      if (count == n) {
        send(this, timestep, isChanged);
        isChanged = false;
        count = 0;
      }
    };

    for (var i = n; i--; ) { args[i].kids.push(this); }

  }

  var keysDown = Signal.dropRepeats(new KeyMerge(downEvents,upEvents,blurEvents));
  var keysDownArr = Signal.dropRepeats(new KeyMergeArr(downEvents, upEvents, blurEvents));

  function keySignal(f) {
    var signal = A2(Signal.lift, f, keysDown);
    // what's the significance of these two following lines? -jpm
    keysDown.defaultNumberOfKids += 1;
    signal.defaultNumberOfKids = 0;
    return signal;
  }

  function keySignalArr(f) {
    var signal = Signal.Uncurried.lift(f, keysDownArr);
    keysDownArr.defaultNumberOfKids += 1;
    signal.defaultNumberOfKids = 0;
    return signal;
  }

  function dir(up, down, left, right) {
    function f(ks) {
      var x = 0, y = 0;
      while (ks.ctor === "::") {
        switch (ks._0) {
          case left : --x; break;
          case right: ++x; break;
          case up   : ++y; break;
          case down : --y; break;
        }
        ks = ks._1;
      }
      return { _:{}, x:x, y:y };
    }
    return keySignal(f);
  }

  function dirArr(up, down, left, right) {
    function f(ks) {
      var x = 0, y = 0;
      var len = ks.length;
      for (var i = 0; i < len; ++i) {
        switch (ks[i]) {
          case left : --x; break;
          case right: ++x; break;
          case up   : ++y; break;
          case down : --y; break;
        }
      }
      return [x, y];
    }
    return keySignalArr(f);
  }

  function is(key) { return keySignal(NList.member(key)); }
  function isArr(key) {
    return keySignal(function(arr){ return arrMember(key, arr); });
  }

  var lastPressed = Signal.dropRepeats(downEvents);

  return elm.Native.Keyboard = {
    isDown:is,
    directions:F4(dir),
    keysDown:keysDown,
    lastPressed:lastPressed,
    
    Elba : {
      isDown : isArr,
      keysDown : keysDownArr,
      lastPressed : lastPressed,
      directions : dirArr
    }
  };

};
