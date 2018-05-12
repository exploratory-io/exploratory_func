library(V8)

bson_objectid_browserified <- "(function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c='function'==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error(\"Cannot find module '\"+i+\"'\");throw a.code='MODULE_NOT_FOUND',a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u='function'==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
(function (global){
global.ObjectID = require('bson-objectid');

}).call(this,typeof global !== 'undefined' ? global : typeof self !== 'undefined' ? self : typeof window !== 'undefined' ? window : {})
},{'bson-objectid':2}],2:[function(require,module,exports){
(function (process){

var MACHINE_ID = parseInt(Math.random() * 0xFFFFFF, 10);
var index = ObjectID.index = parseInt(Math.random() * 0xFFFFFF, 10);
var pid = (typeof process === 'undefined' || typeof process.pid !== 'number' ? Math.floor(Math.random() * 100000) : process.pid) % 0xFFFF;

/**
 * Determine if an object is Buffer
 *
 * Author:   Feross Aboukhadijeh <feross@feross.org> <http://feross.org>
 * License:  MIT
 *
 */
var isBuffer = function (obj) {
  return !!(
  obj != null &&
  obj.constructor &&
  typeof obj.constructor.isBuffer === 'function' &&
  obj.constructor.isBuffer(obj)
  )
};

/**
 * Create a new immutable ObjectID instance
 *
 * @class Represents the BSON ObjectID type
 * @param {String|Number} arg Can be a 24 byte hex string, 12 byte binary string or a Number.
 * @return {Object} instance of ObjectID.
 */
function ObjectID(arg) {
  if(!(this instanceof ObjectID)) return new ObjectID(arg);
  if(arg && ((arg instanceof ObjectID) || arg._bsontype==='ObjectID'))
    return arg;

  var buf;

  if(isBuffer(arg) || (Array.isArray(arg) && arg.length===12)) {
    buf = Array.prototype.slice.call(arg);
  }
  else if(typeof arg === 'string') {
    if(arg.length!==12 && !ObjectID.isValid(arg))
      throw new Error('Argument passed in must be a single String of 12 bytes or a string of 24 hex characters');

    buf = buffer(arg);
  }
  else if(/number|undefined/.test(typeof arg)) {
    buf = buffer(generate(arg));
  }

  Object.defineProperty(this, 'id', {
    enumerable: true,
    get: function() { return String.fromCharCode.apply(this, buf); }
  });
  Object.defineProperty(this, 'str', {
    get: function() { return buf.map(hex.bind(this, 2)).join(''); }
  });
}
module.exports = ObjectID;
ObjectID.generate = generate;

/**
 * Creates an ObjectID from a second based number, with the rest of the ObjectID zeroed out. Used for comparisons or sorting the ObjectID.
 *
 * @param {Number} time an integer number representing a number of seconds.
 * @return {ObjectID} return the created ObjectID
 * @api public
 */
ObjectID.createFromTime = function(time){
  time = parseInt(time, 10) % 0xFFFFFFFF;
  return new ObjectID(hex(8,time)+'0000000000000000');
};

/**
 * Creates an ObjectID from a hex string representation of an ObjectID.
 *
 * @param {String} hexString create a ObjectID from a passed in 24 byte hexstring.
 * @return {ObjectID} return the created ObjectID
 * @api public
 */
ObjectID.createFromHexString = function(hexString) {
  if(!ObjectID.isValid(hexString))
    throw new Error('Invalid ObjectID hex string');

  return new ObjectID(hexString);
};

/**
 * Checks if a value is a valid bson ObjectId
 *
 * @param {String} objectid Can be a 24 byte hex string or an instance of ObjectID.
 * @return {Boolean} return true if the value is a valid bson ObjectID, return false otherwise.
 * @api public
 *
 * THE NATIVE DOCUMENTATION ISN'T CLEAR ON THIS GUY!
 * http://mongodb.github.io/node-mongodb-native/api-bson-generated/objectid.html#objectid-isvalid
 */
ObjectID.isValid = function(objectid) {
  if(!objectid) return false;

  //call .toString() to get the hex if we're
  // working with an instance of ObjectID
  return /^[0-9A-F]{24}$/i.test(objectid.toString());
};

ObjectID.prototype = {
  _bsontype: 'ObjectID',
  constructor: ObjectID,

  /**
   * Return the ObjectID id as a 24 byte hex string representation
   *
   * @return {String} return the 24 byte hex string representation.
   * @api public
   */
  toHexString: function() {
    return this.str;
  },

  /**
   * Compares the equality of this ObjectID with `otherID`.
   *
   * @param {Object} other ObjectID instance to compare against.
   * @return {Boolean} the result of comparing two ObjectID's
   * @api public
   */
  equals: function (other){
    return !!other && this.str === other.toString();
  },

  /**
   * Returns the generation date (accurate up to the second) that this ID was generated.
   *
   * @return {Date} the generation date
   * @api public
   */
  getTimestamp: function(){
    return new Date(parseInt(this.str.substr(0,8), 16) * 1000);
  }
};

function next() {
  return index = (index+1) % 0xFFFFFF;
}

function generate(time) {
  if (typeof time !== 'number')
    time = Date.now()/1000;

  //keep it in the ring!
  time = parseInt(time, 10) % 0xFFFFFFFF;

  //FFFFFFFF FFFFFF FFFF FFFFFF
  return hex(8,time) + hex(6,MACHINE_ID) + hex(4,pid) + hex(6,next());
}

function hex(length, n) {
  n = n.toString(16);
  return (n.length===length)? n : '00000000'.substring(n.length, length) + n;
}

function buffer(str) {
  var i=0,out=[];

  if(str.length===24)
    for(;i<24; out.push(parseInt(str[i]+str[i+1], 16)),i+=2);

  else if(str.length===12)
    for(;i<12; out.push(str.charCodeAt(i)),i++);

  return out;
}

/**
 * Converts to a string representation of this Id.
 *
 * @return {String} return the 24 byte hex string representation.
 * @api private
 */
ObjectID.prototype.inspect = function() { return 'ObjectID('+this+')' };
ObjectID.prototype.toJSON = ObjectID.prototype.toHexString;
ObjectID.prototype.toString = ObjectID.prototype.toHexString;

}).call(this,require('_process'))
},{'_process':3}],3:[function(require,module,exports){
// shim for using process in browser
var process = module.exports = {};

// cached from whatever global is present so that test runners that stub it
// don't break things.  But we need to wrap it in a try catch in case it is
// wrapped in strict mode code which doesn't define any globals.  It's inside a
// function because try/catches deoptimize in certain engines.

var cachedSetTimeout;
var cachedClearTimeout;

function defaultSetTimout() {
    throw new Error('setTimeout has not been defined');
}
function defaultClearTimeout () {
    throw new Error('clearTimeout has not been defined');
}
(function () {
    try {
        if (typeof setTimeout === 'function') {
            cachedSetTimeout = setTimeout;
        } else {
            cachedSetTimeout = defaultSetTimout;
        }
    } catch (e) {
        cachedSetTimeout = defaultSetTimout;
    }
    try {
        if (typeof clearTimeout === 'function') {
            cachedClearTimeout = clearTimeout;
        } else {
            cachedClearTimeout = defaultClearTimeout;
        }
    } catch (e) {
        cachedClearTimeout = defaultClearTimeout;
    }
} ())
function runTimeout(fun) {
    if (cachedSetTimeout === setTimeout) {
        //normal enviroments in sane situations
        return setTimeout(fun, 0);
    }
    // if setTimeout wasn't available but was latter defined
    if ((cachedSetTimeout === defaultSetTimout || !cachedSetTimeout) && setTimeout) {
        cachedSetTimeout = setTimeout;
        return setTimeout(fun, 0);
    }
    try {
        // when when somebody has screwed with setTimeout but no I.E. maddness
        return cachedSetTimeout(fun, 0);
    } catch(e){
        try {
            // When we are in I.E. but the script has been evaled so I.E. doesn't trust the global object when called normally
            return cachedSetTimeout.call(null, fun, 0);
        } catch(e){
            // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error
            return cachedSetTimeout.call(this, fun, 0);
        }
    }


}
function runClearTimeout(marker) {
    if (cachedClearTimeout === clearTimeout) {
        //normal enviroments in sane situations
        return clearTimeout(marker);
    }
    // if clearTimeout wasn't available but was latter defined
    if ((cachedClearTimeout === defaultClearTimeout || !cachedClearTimeout) && clearTimeout) {
        cachedClearTimeout = clearTimeout;
        return clearTimeout(marker);
    }
    try {
        // when when somebody has screwed with setTimeout but no I.E. maddness
        return cachedClearTimeout(marker);
    } catch (e){
        try {
            // When we are in I.E. but the script has been evaled so I.E. doesn't  trust the global object when called normally
            return cachedClearTimeout.call(null, marker);
        } catch (e){
            // same as above but when it's a version of I.E. that must have the global object for 'this', hopfully our context correct otherwise it will throw a global error.
            // Some versions of I.E. have different rules for clearTimeout vs setTimeout
            return cachedClearTimeout.call(this, marker);
        }
    }



}
var queue = [];
var draining = false;
var currentQueue;
var queueIndex = -1;

function cleanUpNextTick() {
    if (!draining || !currentQueue) {
        return;
    }
    draining = false;
    if (currentQueue.length) {
        queue = currentQueue.concat(queue);
    } else {
        queueIndex = -1;
    }
    if (queue.length) {
        drainQueue();
    }
}

function drainQueue() {
    if (draining) {
        return;
    }
    var timeout = runTimeout(cleanUpNextTick);
    draining = true;

    var len = queue.length;
    while(len) {
        currentQueue = queue;
        queue = [];
        while (++queueIndex < len) {
            if (currentQueue) {
                currentQueue[queueIndex].run();
            }
        }
        queueIndex = -1;
        len = queue.length;
    }
    currentQueue = null;
    draining = false;
    runClearTimeout(timeout);
}

process.nextTick = function (fun) {
    var args = new Array(arguments.length - 1);
    if (arguments.length > 1) {
        for (var i = 1; i < arguments.length; i++) {
            args[i - 1] = arguments[i];
        }
    }
    queue.push(new Item(fun, args));
    if (queue.length === 1 && !draining) {
        runTimeout(drainQueue);
    }
};

// v8 likes predictible objects
function Item(fun, array) {
    this.fun = fun;
    this.array = array;
}
Item.prototype.run = function () {
    this.fun.apply(null, this.array);
};
process.title = 'browser';
process.browser = true;
process.env = {};
process.argv = [];
process.version = ''; // empty string to avoid regexp issues
process.versions = {};

function noop() {}

process.on = noop;
process.addListener = noop;
process.once = noop;
process.off = noop;
process.removeListener = noop;
process.removeAllListeners = noop;
process.emit = noop;
process.prependListener = noop;
process.prependOnceListener = noop;

process.listeners = function (name) { return [] }

process.binding = function (name) {
    throw new Error('process.binding is not supported');
};

process.cwd = function () { return '/' };
process.chdir = function (dir) {
    throw new Error('process.chdir is not supported');
};
process.umask = function() { return 0; };

},{}]},{},[1]);"

jsToMongoJson <- function(js) {
  # Create V8 javascript context.
  ct <- v8()

  ct$eval(bson_objectid_browserified)
  # Define necessary js functions.
  ct$assign("MinKey", JS("{
      $minKey: 1
  }"))
  ct$assign("MaxKey", JS("{
      $maxKey: 1
  }"))
  ct$assign("NumberLong", JS("function(numberStr) {
      return {$numberLong: numberStr.toString()};
  }"))
  ct$assign("NumberInt", JS("function(number) {
      // Looks like $numberInt does not exist.
      // Work it around by using $numberLong instead.
      // When use in query condition, it should not make difference.
      // https://jira.mongodb.org/browse/SERVER-11957
      return {$numberLong: number.toString()};
  }"))
  ct$assign("ObjectId", JS("function(id) {
      var oid = ObjectID(id);
      // Mark the object as a mock ObjectId.
      // We do this because the following does not work
      // to tell this mock ObjectId object apart from other objects
      // when constructor.name is minified into something like 'n'.
      // obj[key].constructor.name === 'ObjectID'
      oid.mongo_objectid_mock = true;
      return oid;
  }"))
  ct$assign("DBRef", JS("function(name, id) {
      return {$ref: name, $id: id};
  }"))
  ct$assign("Timestamp", JS("function(t, i) {
      return {$timestamp: {t:t, i:i}};
  }"))
  ct$assign("BinData", JS("function(t, bindata) {
      // This function returns the following same object for both BinData() and new BinData().
      return {$binary: bindata, $type:t.toString(16)};
  }"))

  ct$assign("ISODate", JS("function(dateStr) {
    return new Date(dateStr);
  }"))

  ct$assign("UUID", JS("function(uuidHexStr) {
    return context.BinData(3, new Buffer(uuidHexStr,'hex').toString('base64'));
  }"))

  
  ct$assign("traverse", JS("function(x) {
    if (isArray(x) || ((typeof x === 'object') && (x !== null))) {
      traverseObject(x);
    } else {
      // no need to go deeper.
    }
    return x;
  }"))
  
  # Checks if an object is an Array. Part of object tree travarsal scheme.
  ct$assign("isArray", JS("function(o) {
    return Object.prototype.toString.call(o) === '[object Array]';
  }"))
  
  # Traverse array or object. Part of object tree travarsal scheme.
  ct$assign("traverseObject", JS("function(obj) {
    for (var key in obj) {
      if (obj.hasOwnProperty(key)) {
        // Use following debug log to trace type of visited objects.
        // console.info('Visiting object in mongo query: ', obj[key], obj[key].constructor && obj[key].constructor.name);
        if (obj[key] === undefined){
          obj[key] = {$undefined: true};
        }
        else if (obj[key] === null){
          // if null, accessing attribute of it like constructor results in error. so just skip.
        }
        else if (obj[key].constructor && obj[key].constructor.name === 'Date') {
          // It seems to be ok to use 'Z' as timezone just as toISOString() does for $date
          // even though this document says the timezone format is <+/-Offset>.
          // https://docs.mongodb.com/v3.2/reference/mongodb-extended-json/
            obj[key] = {$date: obj[key].toISOString()};
        }
        else if ((obj[key].constructor && obj[key].constructor.name === 'RegExp') || obj[key].$regex) {
          obj[key] = regexToMongoJson(obj[key]);
        }
        else if (obj[key].mongo_objectid_mock) {
          obj[key] = {$oid: obj[key].toString()};
        }
        else {
          traverse(obj[key]);
        }
      }
    }
  }"))
  
  # Converts js RegExp object to mongo json.
  ct$assign("regexToMongoJson", JS("function(regex) {
    if (regex.$regex) {
      // Take care of following cases where $regex is already there, but with regex object rather than string.
      // { $regex: /pattern/, $options: '<options>' }
      // { $regex: /pattern/<options> }
      var inner_regex = regex.$regex;
      if (inner_regex.constructor && inner_regex.constructor.name === 'RegExp') {
        return {$regex:inner_regex.source, $options:(regex.$options || inner_regex.flags)};
      }
      else {
        // Probably it is already mongo json format. Return it as is.
        return regex;
      }
    }
    else {
      return {$regex:regex.source, $options:regex.flags};
    }
  }"))
  
  ct$eval(paste0(c('JSON.stringify(traverse(', js, '))')))
}
