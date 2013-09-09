
Elm.Signal = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  if (elm.Native.Signal) return elm.Native.Signal;

  var Utils = Elm.Native.Utils(elm);
  var foldl1 = Elm.Native.List(elm).foldl1;

  function send(node, timestep, changed) {
    var kids = node.kids;
    for (var i = kids.length; i--; ) {
      kids[i].recv(timestep, changed, node.id);
    }
  }

  function Input(base) {
    this.id = Utils.guid();
    this.value = base;
    this.kids = [];
    this.defaultNumberOfKids = 0;
    this.recv = function(timestep, eid, v) {
      var changed = eid === this.id;
      if (changed) { this.value = v; }
      send(this, timestep, changed);
      return changed;
    };
    elm.inputs.push(this);
  }

  function LiftN(update, args) {
    this.id = Utils.guid();
    this.value = update();
    this.kids = [];

    var n = args.length;
    var count = 0;
    var isChanged = false;

    this.recv = function(timestep, changed, parentID) {
      ++count;
      if (changed) { isChanged = true; }
      if (count == n) {
        if (isChanged) { this.value = update(); }
        send(this, timestep, isChanged);
        isChanged = false;
        count = 0;
      }
    };
    for (var i = n; i--; ) { args[i].kids.push(this); }
  }

  function lift2Uncurried(func, a, b){
    function update(){ return func(a.value, b.value); }
    return new LiftN(update, [a, b]);
  }

  function lift(func, a) {
    function update() { return func(a.value); }
    return new LiftN(update, [a]);
  }
  function lift2(func, a, b) {
    function update() { return A2( func, a.value, b.value ); }
    return new LiftN(update, [a,b]);
  }
  function lift3(func, a, b, c) {
    function update() { return A3( func, a.value, b.value, c.value ); }
    return new LiftN(update, [a,b,c]);
  }
  function lift4(func, a, b, c, d) {
    function update() { return A4( func, a.value, b.value, c.value, d.value ); }
    return new LiftN(update, [a,b,c,d]);
  }
  function lift5(func, a, b, c, d, e) {
    function update() { return A5( func, a.value, b.value, c.value, d.value, e.value ); }
    return new LiftN(update, [a,b,c,d,e]);
  }
  function lift6(func, a, b, c, d, e, f) {
    function update() { return A6( func, a.value, b.value, c.value, d.value, e.value, f.value ); }
    return new LiftN(update, [a,b,c,d,e,f]);
  }
  function lift7(func, a, b, c, d, e, f, g) {
    function update() { return A7( func, a.value, b.value, c.value, d.value, e.value, f.value, g.value ); }
    return new LiftN(update, [a,b,c,d,e,f,g]);
  }
  function lift8(func, a, b, c, d, e, f, g, h) {
    function update() { return A8( func, a.value, b.value, c.value, d.value, e.value, f.value, g.value, h.value ); }
    return new LiftN(update, [a,b,c,d,e,f,g,h]);
  }

  function Foldp(step, state, input) {
    this.id = Utils.guid();
    this.value = state;
    this.kids = [];

    this.recv = function(timestep, changed, parentID) {
      if (changed) {
          this.value = A2( step, input.value, this.value );
      }
      send(this, timestep, changed);
    };
    input.kids.push(this);
  }

  // Uncurried version
  function Scan(step, state, input) {
    this.id = Utils.guid();
    this.value = state;
    this.kids = [];

    this.recv = function(timestep, changed, parentID) {
      if (changed) {
        // arguments were in the wrong order originally!!!
          this.value = step(this.value, input.value);
      }
      send(this, timestep, changed);
    };
    input.kids.push(this);
  }


  function removeFromKids(input, idToRemove){
    input.kids = input.kids.filter(function(k){
      return (k.id !== idToRemove);
    });
  }

  function Switcher(signalSignal){
    this.id = Utils.guid();
    var self = this;
    this.kids = [];
    var curr = signalSignal.value;
    this.value = curr.value;

    this.recv = function(timestep, changed, parentId) {
      if (parentId === signalSignal.id){
        if (changed) {
          removeFromKids(curr, self.id);
          curr = signalSignal.value;
          this.value = curr.value;
          curr.kids.push(this);
        }
      } else if (parentId === curr.id){
        if (changed) {
          this.value = curr.value;
        }
      } else {
        console.log("I'll be reading this soon");
      }
      send(this, timestep, changed);
    };

    signalSignal.kids.push(this);
    curr.kids.push(this);
  }

  function findValueById(signals, id) {
    var len = signals.length;
    for (var i = 0; i < len; ++i){
      var sig = signals[i];
      if (sig.id === id){
        return sig.value;
      }
    }
    console.log('damn');
  }

  function Join(f, signalSignal){
    this.id = Utils.guid();
    var feeders = [signalSignal.value];
    this.value = signalSignal.value.value;
    this.kids = [];

    this.recv = function(timestep, changed, parentId) {
      if (changed){
        if (parentId === signalSignal.id){
          feeders.push(signalSignal.value);
          signalSignal.value.kids.push(this);
        } else {
          this.value = findValueById(feeders, parentId);
        }
      }
      send(this, timestep, changed);
    };
    signalSignal.kids.push(this);
  }

  function join(f, signalSignal) {
    return new join(f, signalSignal);
  }

  function Bind(f, input) {
    this.id = Utils.guid();
    var feeders = [f(input.value)];
    feeders[0].kids.push(this);
    this.value = feeders[0].value;
    this.kids = [];

    this.recv = function(timestep, changed, parentId) {
      if (changed) {
        if (parentId === input.id) {
          var newSignal = f(input.value);
          newSignal.kids.push(this);
          feeders.push(newSignal);
        } else {
          this.value = findValueById(feeders, parentId);
        }
      }
      send(this, timestep, changed);
    };
    input.kids.push(this);
  }

  function bind(f, input) {
    return new Bind(f, input);
  }

  function switcher(signalSignal) {
    return new Switcher(signalSignal);
  }

  function foldp(step, state, input) {
      return new Foldp(step, state, input);
  }

  function Sink(f, input) {
    this.id = Utils.guid();
    this.recv = function(timestep, changed, parentID) {
      if (changed) {
        f(input.value);
      }
    };
    input.kids.push(this);
    var myId = this.id;
    this.unsubscribe = function(){
      removeFromKids(input, myId);
    };
  }

  function sink(f, input) {
    return new Sink(f, input);
  }

  function DropIf(pred,base,input) {
    this.id = Utils.guid();
    this.value = pred(input.value) ? base : input.value;
    this.kids = [];
    this.recv = function(timestep, changed, parentID) {
      var chng = changed && !pred(input.value);
      if (chng) { this.value = input.value; }
      send(this, timestep, chng);
    };
    input.kids.push(this);
  }

  function DropRepeatsEq(eq, input) {
    this.id = Utils.guid();
    this.value = input.value;
    this.kids = [];
    this.recv = function(timestep, changed, parentID) {
      var chng = changed && !eq(this.value,input.value);
      if (chng) { this.value = input.value; }
      send(this, timestep, chng);
    };
    input.kids.push(this);
  }

  function DropRepeats(input) {
    this.id = Utils.guid();
    this.value = input.value;
    this.kids = [];
    this.recv = function(timestep, changed, parentID) {
      var chng = changed && !Utils.eq(this.value,input.value);
      if (chng) { this.value = input.value; }
      send(this, timestep, chng);
    };
    input.kids.push(this);
  }

  function dropWhen(s1,b,s2) {
    var pairs = lift2( F2(function(x,y){return {x:x,y:y};}), s1, s2 );
    var dropped = new DropIf(function(p){return p.x;},{x:true,y:b},pairs);
    return lift(function(p){return p.y;}, dropped);
  }

  function timestamp(a) {
    function update() { return Utils.Tuple2(Date.now(), a.value); }
    return new LiftN(update, [a]);
  }

  function SampleOn(s1,s2) {
    this.id = Utils.guid();
    this.value = s2.value;
    this.kids = [];

    var count = 0;
    var isChanged = false;

    this.recv = function(timestep, changed, parentID) {
      if (parentID === s1.id) isChanged = changed;
      ++count;
      if (count == 2) {
        if (isChanged) { this.value = s2.value; }
        send(this, timestep, isChanged);
        count = 0;
        isChanged = false;
      }
    };
    s1.kids.push(this);
    s2.kids.push(this);
  }

  function sampleOn(s1,s2) { return new SampleOn(s1,s2); }

  function delay(t,s) {
      var delayed = new Input(s.value);
      var firstEvent = true;
      function update(v) {
        if (firstEvent) { firstEvent = false; return; }
        setTimeout(function() { elm.notify(delayed.id, v); }, t);
      }
      function first(a,b) { return a; }
      return new SampleOn(delayed, lift2(F2(first), delayed, lift(update,s)));
  }

  function Merge(s1,s2) {
      this.id = Utils.guid();
      this.value = s1.value;
      this.kids = [];

      var next = null;
      var count = 0;
      var isChanged = false;

      this.recv = function(timestep, changed, parentID) {
        ++count;
        if (changed) {
            isChanged = true;
            if (parentID == s2.id && next === null) { next = s2.value; }
            if (parentID == s1.id) { next = s1.value; }
        }

        if (count == 2) {
            if (isChanged) { this.value = next; next = null; }
            send(this, timestep, isChanged);
            isChanged = false;
            count = 0;
        }
      };
      s1.kids.push(this);
      s2.kids.push(this);
  }

  function merge(s1,s2) { return new Merge(s1,s2); }
  function merges(ss) { return A2(foldl1, F2(merge), ss); }

  return elm.Native.Signal = {
    constant : function(v) { return new Input(v); },
    lift  : F2(lift ),
    lift2 : F3(lift2),
    lift3 : F4(lift3),
    lift4 : F5(lift4),
    lift5 : F6(lift5),
    lift6 : F7(lift6),
    lift7 : F8(lift7),
    lift8 : F9(lift8),
    Uncurried : {
      constant : function(v){ return new Input(v); },
      lift : lift,
      lift2 : lift2Uncurried,
      dropIf : function(pred, base, sig){ return new DropIf(pred, base, sig); },
      dropWhen : dropWhen,
      keepWhen : function(s1, b, s2){
        return dropWhen(lift(function(b){return !b;}, s1), b, s2);},
      switcher : switcher,
      scan: function(step, state, sig){ return new Scan(step, state, sig); },
      join : join,
      bind : bind,
      merge : merge,
      delay : delay,
      sink : sink,
      sampleOn : sampleOn,
      dropRepeatsEq : function(eq, s){ return new DropRepeatsEq(eq, s); }
    },
    foldp : F3(foldp),
    delay : F2(delay),
    merge : F2(merge),
    merges : merges,
    count : function(s) { return foldp(F2(function(_,c) { return c+1; }), 0, s); },
    countIf : F2(function(pred,s) {
      return foldp(F2(function(x,c){
        return pred(x) ? c+1 : c; }), 0, s)}),
    keepIf : F3(function(pred,base,sig) {
      return new DropIf(function(x) {return !pred(x);},base,sig); }),
    dropIf : F3(function(pred,base,sig) { return new DropIf(pred,base,sig); }),
    keepWhen : F3(function(s1,b,s2) {
      return dropWhen(lift(function(b){return !b;},s1), b, s2); }),
    dropWhen : F3(dropWhen),
    dropRepeats : function(s) { return new DropRepeats(s);},
    sampleOn : F2(sampleOn),
    timestamp : timestamp,
    join : F2(join),
    switcher: switcher
  };
};
