/* global exports */
"use strict";

// module Typeahead

exports.typeahead = function(ob) {
  return function(opts) {
    return function(datasets) {
      return function() {
        return ob.typeahead(opts, datasets);
      };
    };
  };
};

exports.mkSource = function (sourceEff) {
  return function(q, syncCallback, asyncCallback) {
    return sourceEff(q)
      (function(res) {
        return function() {
          return syncCallback(res);
        };
      })
      (function(res) {
        return function() {
          return asyncCallback(res);
        };
      })();
  }
}

exports.getVal = function(ob) {
  return function() {
    return ob.typeahead('val');
  }
};

exports.setVal = function(ob) {
  return function(val) {
    return function() {
      return ob.typeahead('val', val);
    }
  }
};

exports.open = function(ob) {
  return function() {
    return ob.typeahead('open');
  }
};

exports.close = function(ob) {
  return function() {
    return ob.typeahead('close');
  }
};

exports.destroy = function(ob) {
  return function() {
    return ob.typeahead('destroy');
  }
};

exports.bindEventImpl1 = function(name) {
  return function(ta) {
    return function(act) {
      return function() {
        return ta.on(name, function(e) {
          act(e)();
        });
      }
    }
  }
}

exports.bindEventImpl2 = function(name) {
  return function(ta) {
    return function(act) {
      return function() {
        return ta.on(name, function(e, arg1) {
          act(e)(arg1)();
        });
      }
    }
  }
}
