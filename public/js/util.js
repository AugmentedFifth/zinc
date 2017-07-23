/**
 * Custom implementation of `Array.prototype.toString()`.
 *
 * @return {string}
 */
Array.prototype.toString = function() {
    "use strict";
    return `[${this.join(", ")}]`;
};


/* ================| Circular buffer implementation |================ */


/**
 * Creates a new `CircularBuffer` with the specified `capacity`,
 * which must strictly be a positive integer.
 *
 * We here use the term "zero element", which refers to exactly `null` if the
 * `CircularBuffer` is backed by an `Array`, and refers to exactly `0` if it is
 * backed by a `TypedArray`. Note that using this constructor will always yield
 * a `CircularBuffer` backed by an `Array`. Use the
 * `CircularBuffer.fromArray()` function any time a `TypedArray` backing is
 * required.
 *
 * The `head` and `tail` both start at `0`. The `inhabited` property is a count
 * of how many non-zero elements there are in the buffer.
 *
 * @template T - The type of object that the `CircularBuffer` will store.
 * @param {number} capacity
 * @return {CircularBuffer<T>}
 */
function CircularBuffer(capacity) {
    "use strict";
    if (typeof capacity !== "number" || capacity < 1 || capacity % 1 !== 0) {
        throw new Error(
            "CircularBuffer: capacity must be a positive integer. " +
                `Got: ${capacity}`
        );
    }

    /** **_Internal use only._** */
    this._buffer = Array(capacity).fill(null);

    this.capacity = capacity;
    this.head = 0;
    this.tail = 0;
    this.inhabited = 0;
    this.isArray = true;
}

/**
 * Creates a new `CircularBuffer` from the given `Array` or `TypedArray`. The
 * array cannot be empty. Note that `TypedArray`s will always have an
 * `inhabited` property equal to the number of values that they hold that
 * aren't `0`, since `TypedArray`s store primitives and thus cannot be
 * `null`ed.
 *
 * @template T - The type of object that the `CircularBuffer` will store.
 * @param {T[]} array - The array to make the `CircularBuffer` from.
 * @return {CircularBuffer<T>}
 */
CircularBuffer.fromArray = function(array) {
    "use strict";
    const isArray = Array.isArray(array);
    if (isArray && array.length < 1) {
        throw new Error("CircularBuffer.fromArray: array cannot be empty.");
    }
    if (
        !isArray && !(
            ArrayBuffer.isView(array) && !(array instanceof DataView)
        )
    ) {
        throw new Error(
            "CircularBuffer.fromArray: array must be an Array or a Typed" +
                `Array. Got: ${array}`
        );
    }
    const cb = new CircularBuffer(array.length);
    cb._buffer = array;
    const zeroElement = isArray ? null : 0;
    cb.inhabited = array.reduce(
        (count, val) => val === zeroElement ? count : count + 1,
        0
    );
    cb.isArray = isArray;
    return cb;
};

/**
 * Returns a `string` representation of the `CircularBuffer`, in the form
 * "[object CircularBuffer(`capacity`) capacity `capacity` head `head` tail
 * `tail` inhabited `inhabited` isArray `isArray`]"
 *
 * @return {string} - A `string` representation of the `CircularBuffer`.
 */
CircularBuffer.prototype.toString = function() {
    "use strict";
    return `[object CircularBuffer(${this.capacity}) capacity ` +
           `${this.capacity} head ${this.head} tail ${this.tail} inhabited ` +
           `${this.inhabited} isArray ${this.isArray}]`;
};

/**
 * "Cons"es the given element at the `head` of the `CircularBuffer`, thus
 * placing this new element in the old `head`'s position and moving the `head`
 * cursor up by one. The old value at the overwritten position is returned
 * (the zero element if it was uninhabited).
 *
 * If the old `head` was overlapping the `tail` and that spot was inhabited,
 * then the `tail` is shifted forward by one as well.
 *
 * @template T
 * @param {T} val - The value to "cons" onto the `CircularBuffer`.
 * @return {T} - The old value that was at the "cons"ed position, the zero
 *               element if it was uninhabited.
 */
CircularBuffer.prototype.cons = function(val) {
    "use strict";
    const oldVal = this._buffer[this.head];
    this._buffer[this.head] = val;
    const zeroElement = this.isArray ? null : 0;
    if (oldVal === zeroElement) {
        this.inhabited++;
    }
    if (this.head === this.tail && oldVal !== zeroElement) {
        this._increment_tail();
    }
    this._increment_head();
    return oldVal;
};

/**
 * Alias for the `CircularBuffer.cons()` method.
 */
CircularBuffer.prototype.push = CircularBuffer.prototype.cons;

/**
 * Acts as the inverse of `CircularBuffer.cons()`, removing and returning the
 * element at the `head` of the buffer (minus one), pushing the `head` cursor
 * back by 1.
 *
 * **Note** that this will function even if the "element" being unconsed is
 * actually uninhabited (i.e. it is the zero element).
 *
 * @template T
 * @return {T} - The "uncons"ed value, the zero element if it was uninhabited.
 */
CircularBuffer.prototype.uncons = function() {
    "use strict";
    this._decrement_head();
    const oldVal = this._buffer[this.head];
    const zeroElement = this.isArray ? null : 0;
    if (oldVal !== zeroElement) {
        this.inhabited--;
    }
    this._buffer[this.head] = zeroElement;
    return oldVal;
};

/**
 * Alias for the `CircularBuffer.uncons()` method.
 */
CircularBuffer.prototype.pop = CircularBuffer.prototype.cons;

/**
 * Acts like `CircularBuffer.cons()`, but puts an element onto the `tail` of
 * the buffer instead of the `head`, pushing back `tail` by 1.
 *
 * If `tail` pointed to the same spot as `head` before the "snoc", then `head`
 * is also pushed back by 1.
 *
 * @template T
 * @param {T} val - The value to "snoc" onto the `CircularBuffer`.
 * @return {T} - The old value that was at the "snoc"ed position, the zero
 *               element if it was uninhabited.
 */
CircularBuffer.prototype.snoc = function(val) {
    "use strict";
    if (this.tail === this.head) {
        this._decrement_head();
    }
    this._decrement_tail();
    const oldVal = this._buffer[this.tail];
    const zeroElement = this.isArray ? null : 0;
    if (oldVal === zeroElement) {
        this.inhabited++;
    }
    this._buffer[this.tail] = val;
    return oldVal;
};

/**
 * Alias for the `CircularBuffer.snoc()` method.
 */
CircularBuffer.prototype.shift = CircularBuffer.prototype.snoc;

/**
 * Acts as the inverse of `CircularBuffer.snoc()`, removing and returning the
 * element at the `tail` of the buffer, advancing the `tail` cursor by 1.
 *
 * If `tail` and `head` pointed to the same spot before the "unsnoc", then the
 * `head` cursor is also advanced by 1.
 *
 * @template T
 * @return {T} - The "unsnoc"ed value, the zero element if it was uninhabited.
 */
CircularBuffer.prototype.unsnoc = function() {
    "use strict";
    if (this.tail === this.head) {
        this._increment_head();
    }
    const oldVal = this._buffer[this.tail];
    const zeroElement = this.isArray ? null : 0;
    if (oldVal !== zeroElement) {
        this.inhabited--;
    }
    this._buffer[this.tail] = zeroElement;
    this._increment_tail();
    return oldVal;
};

/**
 * Alias for the `CircularBuffer.unsnoc()` method.
 */
CircularBuffer.prototype.unshift = CircularBuffer.prototype.unsnoc;

(function() {
"use strict";

/**
 * Gets the value at a specified index in the `CircularBuffer`'s stack.
 * The index supplied (`i`) must be an integer, and this method **interprets
 * the index relative to the current `head` cursor position** (minus one,
 * actually). This represents the element last inserted, i.e. LIFO behavior.
 *
 * To get an element relative to the `tail` cursor position (FIFO), use
 * `CircularBuffer.peek()`. To get an element via its position in the
 * underlying buffer, use `CircularBuffer.bufferGet()`.
 *
 * `i = 0` is the default value of `i`, and gets the element at the current
 * `head` cursor position minus one (i.e. the last element inserted into the
 * `CircularBuffer`). Positive values of `i` walk from the last inserted
 * element to the second-to-last inserted element, and so on.
 *
 * Negative indices walk backward, and going off the end of the buffer wraps
 * circularly as expected.
 *
 * @template T
 * @param {number=} i - The index to get from.
 * @return {T} - An element gotten from the `CircularBuffer`.
 */
CircularBuffer.prototype.get = function(i=0) {
    if (typeof i !== "number" || i % 1 !== 0) {
        throw new Error(
            `CircularBuffer.get(): i must be an integer. Got: ${i}`
        );
    }
    return this._buffer[this._clamp_index(this.head - 1 - i)];
};

})();

(function() {
"use strict";

/**
 * Gets the value at a specified index in the `CircularBuffer`'s queue.
 * The index supplied (`i`) must be an integer, and this method **interprets
 * the index relative to the current `tail` cursor position**. This represents
 * the oldest element inserted, i.e. FIFO behavior.
 *
 * To get an element relative to the `head` cursor position (LIFO), use
 * `CircularBuffer.get()`. To get an element via its position in the
 * underlying buffer, use `CircularBuffer.bufferGet()`.
 *
 * `i = 0` is the default value of `i`, and gets the element at the current
 * `tail` cursor position (i.e. the oldest element inserted into the
 * `CircularBuffer`). Positive values of `i` walk from the oldest inserted
 * element to the second-oldest inserted element, and so on.
 *
 * Negative indices walk backward, and going off the end of the buffer wraps
 * circularly as expected.
 *
 * @template T
 * @param {number=} i - The index to get from.
 * @return {T} - An element peeked from the `CircularBuffer`.
 */
CircularBuffer.prototype.peek = function(i=0) {
    if (typeof i !== "number" || i % 1 !== 0) {
        throw new Error(
            `CircularBuffer.peek(): i must be an integer. Got: ${i}`
        );
    }
    return this._buffer[this._clamp_index(this.tail + i)];
};

})();

/**
 * Gets the value at a specified index in the `CircularBuffer`'s buffer.
 * The index supplied (`i`) must be an integer, and this method does **NOT**
 * interpret the index relative to any cursor position (`head` or `tail`); for
 * that, use `CircularBuffer.get()` or `CircularBuffer.peek()`, respectively.
 * Instead, this index indexes directly into the underlying buffer array.
 *
 * Negative indices walk backward, and going off the end of the buffer wraps
 * circularly as expected.
 *
 * @template T
 * @param {number} i - The index to get from.
 * @return {T} - An element gotten from the `CircularBuffer`.
 */
CircularBuffer.prototype.bufferGet = function(i) {
    "use strict";
    if (typeof i !== "number" || i % 1 !== 0) {
        throw new Error(
            `CircularBuffer.bufferGet(): i must be an integer. Got: ${i}`
        );
    }
    return this._buffer[this._clamp_index(i)];
};

/**
 * Behaves like `CircularBuffer.get()`, but sets the value at that position
 * and then returns the old value. Also, `i` is not optional for this method.
 *
 * @template T
 * @param {number} i - The index at which to set the value.
 * @param {T} val - The new value to put at the specified index.
 * @return {T} - The old element that was at that index.
 */
CircularBuffer.prototype.set = function(i, val) {
    "use strict";
    if (typeof i !== "number" || i % 1 !== 0) {
        throw new Error(
            `CircularBuffer.set(): i must be an integer. Got: ${i}`
        );
    }
    const i_ = this._clamp_index(this.head - 1 - i);
    const oldVal = this._buffer[i_];
    const zeroElement = this.isArray ? null : 0;
    if (oldVal === zeroElement && val !== zeroElement) {
        this.inhabited++;
    }
    this._buffer[i_] = val;
    return oldVal;
};

/**
 * Behaves like `CircularBuffer.bufferGet()`, but sets the value at that
 * position and then returns the old value.
 *
 * @template T
 * @param {number} i - The index at which to set the value.
 * @param {T} val - The new value to put at the specified index.
 * @return {T} - The old element that was at that index.
 */
CircularBuffer.prototype.bufferSet = function(i, val) {
    "use strict";
    if (typeof i !== "number" || i % 1 !== 0) {
        throw new Error(
            `CircularBuffer.bufferSet(): i must be an integer. Got: ${i}`
        );
    }
    const i_ = this._clamp_index(i);
    const oldVal = this._buffer[i_];
    const zeroElement = this.isArray ? null : 0;
    if (oldVal === zeroElement && val !== zeroElement) {
        this.inhabited++;
    }
    this._buffer[i_] = val;
    return oldVal;
};

/**
 * Behaves like `CircularBuffer.set()`, but sets the value to the zero element,
 * which represents an uninhabited spot for a `CircularBuffer`.
 *
 * @template T
 * @param {number} i - The index to delete the value at.
 * @return {T} - The old element that was at that index.
 */
CircularBuffer.prototype.delete = function(i) {
    "use strict";
    const zeroElement = this.isArray ? null : 0;
    return this.set(i, zeroElement);
};

/**
 * Behaves like `CircularBuffer.bufferSet()`, but sets the value to the zero
 * element, which represents an uninhabited spot for a `CircularBuffer`.
 *
 * @template T
 * @param {number} i - The index to delete the value at.
 * @return {T} - The old element that was at that index.
 */
CircularBuffer.prototype.bufferDelete = function(i) {
    "use strict";
    const zeroElement = this.isArray ? null : 0;
    return this.bufferSet(i, zeroElement);
};

/**
 * Clears the entire underlying buffer of all its values and resets both the
 * `head` and `tail` cursors.
 *
 * This is equivalent to `delete`ing all elements, and setting
 * `head = tail = 0`.
 *
 * @return {void}
 */
CircularBuffer.prototype.clear = function() {
    "use strict";
    const zeroElement = this.isArray ? null : 0;
    for (let i = 0; i < this.capacity; ++i) {
        this._buffer[i] = zeroElement;
    }
    this.head = 0;
    this.tail = 0;
    this.inhabited = 0;
};

/**
 * Gets a shallow copy of all elements in the buffer from `tail` up to but not
 * including the head element (said element being at `head - 1`).
 *
 * The kind of array that is returned is the same as the kind of the underlying
 * buffer (`this.isArray?`).
 *
 * @template T
 * @return {T[]}
 */
CircularBuffer.prototype.getTail = function() {
    "use strict";
    const tailSize =
        this.tail < this.head ?
            this.head - this.tail - 1 :
            this.capacity - 1 - this.tail + this.head;
    const arr = this.isArray ? [] : new this._buffer.constructor(tailSize);
    let i = this.tail;
    let j = 0;
    for (; i !== this.head - 1; i = (i + 1) % this.capacity) {
        arr[j] = this._buffer[i];
        j++;
    }
    return arr;
};

/**
 * Gets a shallow copy of all elements in the buffer from the head element
 * (at `head - 1`) down to but not including `tail`.
 *
 * The kind of array that is returned is the same as the kind of the underlying
 * buffer (`this.isArray?`).
 *
 * @template T
 * @return {T[]}
 */
CircularBuffer.prototype.getInit = function() {
    "use strict";
    const initSize =
        this.tail < this.head ?
            this.head - this.tail - 1 :
            this.capacity - 1 - this.tail + this.head;
    const arr = this.isArray ? [] : new this._buffer.constructor(initSize);
    let i = this.head - 1;
    let j = initSize - 1;
    for (; i !== this.tail; i = i < 1 ? this.capacity - 1 : i - 1) {
        arr[j] = this._buffer[i];
        j--;
    }
    return arr;
};

/**
 * Gets a shallow copy of the contents of the buffer, as an `Array`.
 *
 * @template T
 * @return {T[]}
 */
CircularBuffer.prototype.asArray = function() {
    "use strict";
    if (this.isArray) {
        return this._buffer.slice();
    }
    const arr = [];
    for (let i = 0; i < this.capacity; ++i) {
        arr.push(this._buffer[i]);
    }
    return arr;
};

(function() {
"use strict";

/**
 * Gets a shallow copy of the contents of the buffer, as a `TypedArray`.
 *
 * When `this.isArray === true`, the type of `TypedArray` to be returned can be
 * specified as an argument to this method (A constructor, like `Int32Array`);
 * the default is `Float64Array`. Otherwise, the type of the underlying buffer
 * is always what is returned.
 *
 * If the elements of this `CircularBuffer` don't "fit" (read: can't be
 * coerced) into the type specified or implied, there will likely be unexpected
 * results and possibly errors. This method makes **no guarantees** about such
 * cases.
 *
 * @param {{new(number): TypedArray}=} type
 * @return {TypedArray<?>}
 */
CircularBuffer.prototype.asTypedArray = function(type=Float64Array) {
    if (this.isArray) {
        const typedArr = new type(this.capacity);
        for (let i = 0; i < this.capacity; ++i) {
            typedArr[i] = this._buffer[i];
        }
        return typedArr;
    }
    return this._buffer.slice();
};

})();

/**
 * Executes the provided function for each element from the tail to the head.
 *
 * @param {function} f
 * @return {void}
 */
CircularBuffer.prototype.forEachTail = function(f) {
    "use strict";
    const zeroElement = this.isArray ? null : 0;
    if (this.head === this.tail && this.peek() === zeroElement) {
        return;
    }
    for (
        let i = this.tail;
        i !== this._clamp_index(this.head - 1);
        i = (i + 1) % this.capacity
    ) {
        f(this._buffer[i]);
    }
};

/**
 * Executes the provided function for each element from the head to the tail.
 *
 * @param {function} f
 * @return {void}
 */
CircularBuffer.prototype.forEachHead = function(f) {
    "use strict";
    let i = this.head - 1;
    for (; i !== this.tail; i = i === 0 ? this.capacity - 1 : i - 1) {
        f(this._buffer[i]);
    }
};

/**
 * Executes the provided function for each element from the beginning to the
 * end of the underlying buffer.
 *
 * @param {function} f
 * @return {void}
 */
CircularBuffer.prototype.forEachBuffer = function(f) {
    "use strict";
    this._buffer.forEach(f);
};

(function() {
"use strict";

/**
 * Maps every value in the underlying buffer to a new value based on the
 * supplied mapping function. **This method does NOT create a new
 * `CircularBuffer`.** It only a returns a reference to `this` to facilitate
 * method chaining.
 *
 * This method takes a second, optional argument, that determines whether the
 * mapping function operates on uninhabited spots or if it leaves them alone.
 * The default behavior is to leave them alone.
 *
 * This method maps elements in the order they exist in the underlying buffer,
 * so don't rely on the oder respecting `head` and `tail`.
 *
 * @template T
 * @param {function} f - The mapping function.
 * @param {boolean=} mapZeroes - Map uninhabited spots, or no?
 * @return {CircularBuffer<T>} - `this`.
 */
CircularBuffer.prototype.map = function(f, mapZeroes=false) {
    if (mapZeroes) {
        this._buffer = this._buffer.map(f);
    } else {
        const zeroElement = this.isArray ? null : 0;
        this._buffer = this._buffer.map(
            elem => elem === zeroElement ? elem : f(elem)
        );
    }
    return this;
};

})();


/* ====================| Internal functions below |==================== */


/**
 * **_Internal use only._**
 */
CircularBuffer.prototype._increment_head = function() {
    "use strict";
    this.head = (this.head + 1) % this.capacity;
};

/**
 * **_Internal use only._**
 */
CircularBuffer.prototype._increment_tail = function() {
    "use strict";
    this.tail = (this.tail + 1) % this.capacity;
};

/**
 * **_Internal use only._**
 */
CircularBuffer.prototype._decrement_head = function() {
    "use strict";
    this.head = this.head === 0 ? this.capacity - 1 : this.head - 1;
};

/**
 * **_Internal use only._**
 */
CircularBuffer.prototype._decrement_tail = function() {
    "use strict";
    this.tail = this.tail === 0 ? this.capacity - 1 : this.tail - 1;
};

/**
 * **_Internal use only._**
 */
CircularBuffer.prototype._clamp_index = function(i) {
    "use strict";
    const i_ = i % this.capacity;
    if (i_ < 0) {
        return i_ + this.capacity;
    }
    return i_;
};


/* ===================| 2D vector implementation |=================== */


/**
 * Constructor for the type of 2-vectors.
 *
 * @param {number} x - The x entry of the vector.
 * @param {number} y - The y entry of the vector.
 * @return {V2} - A new 2-vector with the supplied entries.
 */
function V2(x, y) {
    "use strict";
    this.x = x;
    this.y = y;
}

/**
 * Error tolerance when checking for equality concerning vectors.
 */
V2.epsilon = 1e-12;

/**
 * Convenience method for making a new vector.
 *
 * @param {number} x - The x entry of the vector.
 * @param {number} y - The y entry of the vector.
 * @return {V2} - A new 2-vector with the supplied entries.
 */
function v2(x, y) {
    "use strict";
    return new V2(x, y);
}

/**
 * Creates a `string` representation of this `V2` and returns it, in the form
 * "V2(`x`, `y`)".
 *
 * @return {string}
 */
V2.prototype.toString = function() {
    "use strict";
    return `V2(${this.x}, ${this.y})`;
};

/**
 * Returns a clone of this vector.
 *
 * @return {V2} - A new 2-vector with the same entries as this one.
 */
V2.prototype.clone = function() {
    "use strict";
    return new V2(this.x, this.y);
};

/**
 * Returns an `Array` representation of this vector; an `Array` of
 * length exactly 2.
 *
 * @return {number[]} - An `Array` of two elements representing this vector.
 */
V2.prototype.array = function() {
    "use strict";
    return [this.x, this.y];
};

/**
 * Makes a `V2` out of the first two elements of an `Array`.
 *
 * @param {number[]} - The `Array` to make a `V2` from.
 * @return {V2} - The new vector.
 */
V2.fromArray = function(array) {
    "use strict";
    return new V2(array[0], array[1]);
};

/**
 * Returns the zero vector.
 *
 * @return {V2} - The zero vector.
 */
V2.zero = function() {
    "use strict";
    return new V2(0, 0);
};

/**
 * Compares two vectors to see if they are the same, within an error of
 * `V2.epsilon`.
 *
 * @param {V2} v - The vector to compare to.
 * @return {boolean} - Are they equal?
 */
V2.prototype.equals = function(v) {
    "use strict";
    return Math.abs(this.x - v.x) < V2.epsilon &&
           Math.abs(this.y - v.y) < V2.epsilon;
};

/**
 * Is this the null vector?
 *
 * @return {boolean} - Is this the null vector?
 */
V2.prototype.null = function() {
    "use strict";
    return Math.abs(this.x) < V2.epsilon && Math.abs(this.y) < V2.epsilon;
};

/**
 * Adds two 2-vectors.
 *
 * @param {V2} v - The vector to be added to this one.
 * @return {V2} - A new vector representing the sum.
 */
V2.prototype.add = function(v) {
    "use strict";
    return new V2(this.x + v.x, this.y + v.y);
};

/**
 * Subtracts the supplied vector from this vector, returning the new resulting
 * `V2`.
 *
 * @param {V2} v - The vector to be subtracted from this one.
 * @return {V2} - A new vector representing the difference.
 */
V2.prototype.sub = function(v) {
    "use strict";
    return new V2(this.x - v.x, this.y - v.y);
};

/**
 * Multiplies the supplied scalar by this vector, returning a new `V2` as the
 * result.
 *
 * @param {number} k - The scalar to be multiplied to this one.
 * @return {V2} - A new vector representing the scaled version of this vector.
 */
V2.prototype.scalarMult = function(k) {
    "use strict";
    return new V2(k * this.x, k * this.y);
};

/**
 * Divides this vector by the supplied scalar, returning a new `V2` as the
 * result.
 *
 * @param {number} k - The scalar to divide this vector by.
 * @return {V2} - A new vector representing the scaled version of this vector.
 */
V2.prototype.scalarDiv = function(k) {
    "use strict";
    return new V2(this.x / k, this.y / k);
};

/**
 * Dots this vector by the supplied vector, returning a new `V2` as the result.
 *
 * @param {V2} v - The vector to dot this vector by.
 * @return {number} - A scalar representing the dot product.
 */
V2.prototype.dot = function(v) {
    "use strict";
    return this.x * v.x + this.y * v.y;
};

/**
 * The euclidian norm.
 *
 * @return {number} - The euclidian norm.
 */
V2.prototype.norm = function() {
    "use strict";
    return Math.sqrt(this.x * this.x + this.y * this.y);
};

/**
 * The quadrance.
 *
 * @return {number} - The quadrance.
 */
V2.prototype.quadrance = function() {
    "use strict";
    return this.x * this.x + this.y * this.y;
};

/**
 * Returns a new `V2` representing the closest perpendicular vector (with the
 * same norm) to this one, going anticlockwise.
 *
 * @return {V2} - The nearest same-norm perpendicular vector.
 */
V2.prototype.perp = function() {
    "use strict";
    return new V2(-this.y, this.x);
};

/**
 * Returns a unit-norm `V2` that is `theta` away from the positive x-axis,
 * anticlockwise.
 *
 * `theta` is in radians.
 *
 * @param {number} theta - The angle, in radians.
 * @return {V2} - The corresponding vector on the unit circle.
 */
V2.fromAngle = function(theta) {
    "use strict";
    return new V2(Math.cos(theta), Math.sin(theta));
};

/**
 * The z-component of the cross product of this vector with the supplied vector
 * in the xy-plane.
 *
 * @param {V2} v - The other vector to cross with.
 * @return {number} - The z-component of the cross product.
 */
V2.prototype.crossZ = function(v) {
    "use strict";
    return this.x * v.y - this.y * v.x;
};

/**
 * The distance between vectors in metric space.
 *
 * @param {V2} v - The other vector to get the distance to.
 * @return {number} - The distance between that vector and this one.
 */
V2.prototype.dist = function(v) {
    "use strict";
    return this.sub(v).norm();
};

/**
 * Gets the unit vector that is pointing in the same direction as this one.
 * Acts like `.clone()` for null vectors and unit vectors.
 *
 * @return {V2} - A unit vector, or zero vector if this vector is zero.
 */
V2.prototype.normalize = function() {
    "use strict";
    const quad = this.quadrance();
    if (Math.abs(quad) <= V2.epsilon || Math.abs(quad - 1) <= V2.epsilon) {
        return this.clone();
    }
    const norm = this.norm();
    return new V2(this.x / norm, this.y / norm);
};

/**
 * Gets the projection of a vector onto this one.
 *
 * @param {V2} v - The vector to project onto this one.
 * @return {V2} - The resulting projection vector.
 */
V2.prototype.project = function(v) {
    "use strict";
    return this.scalarMult(this.dot(v) / this.quadrance());
};

/**
 * Gets the angle of this vector from the positive x-axis, anticlockwise.
 *
 * Result is in radians and is always between `-pi` and `pi`.
 *
 * @return {number} - The angle of this vector, in radians.
 */
V2.prototype.angle = function() {
    "use strict";
    return Math.acos(this.x / this.norm()) * Math.sign(this.y);
};

/**
 * Maps a function over both elements of this vector, returning a new `V2`.
 *
 * @param {function} f - The mapping function.
 * @return {V2} - A new, mapped vector.
 */
V2.prototype.map = function(f) {
    "use strict";
    return new V2(f(this.x), f(this.y));
};


/* ====================| 2D rectangle implementation |==================== */


/**
 * Creates a new `Rect` with the specified top-left corner, width, and height.
 * These quantities can be specified in three different ways:
 *
 * - Four arguments, all `numbers`, specifying the x and y values of the
 *   top-left corner and width/height, respectively.
 * - Two arguments, both `V2`s.
 * - One argument, a single `Rect` to be cloned.
 *
 * @param {number | V2 | Rect} arg1
 * @param {number | V2 | void} arg2
 * @param {number | void} arg3
 * @param {number | void} arg4
 * @return {Rect}
 */
function Rect(arg1, arg2, arg3, arg4) {
    "use strict";
    if (typeof arg1 === "number") {
        this.x = arg1;
        this.y = arg2;
        this.width = arg3;
        this.height = arg4;
    } else if (arg1 instanceof V2) {
        this.x = arg1.x;
        this.y = arg1.y;
        this.width = arg2.x;
        this.height = arg2.y;
    } else {
        this.x = arg1.x;
        this.y = arg1.y;
        this.width = arg1.width;
        this.height = arg1.height;
    }
}

/**
 * Convenience function for `new Rect()`.
 *
 * @param {number | V2 | Rect} arg1
 * @param {number | V2 | void} arg2
 * @param {number | void} arg3
 * @param {number | void} arg4
 * @return {Rect}
 */
function rect(arg1, arg2, arg3, arg4) {
    "use strict";
    return new Rect(arg1, arg2, arg3, arg4);
}

/**
 * Returns a `string` representation of this `Rect`, in the form
 * "Rect(`x`, `y`, `width`, `height`)".
 *
 * @return {string}
 */
Rect.prototype.toString = function() {
    "use strict";
    return `Rect(${this.x}, ${this.y}, ${this.width}, ${this.height})`;
};

/**
 * Decides if this rectangle contains the given point within it.
 *
 * The point can be specified as two separate `number`s or just a single `V2`.
 *
 * @param {number | V2} arg1
 * @param {number | void} arg2
 * @return {boolean}
 */
Rect.prototype.contains = function(arg1, arg2) {
    "use strict";
    if (typeof arg1 === "number") {
        return arg1 >= this.x &&
               arg2 >= this.y &&
               arg1 <= this.x + this.width &&
               arg2 <= this.y + this.height;
    }
    return arg1.x >= this.x &&
           arg1.y >= this.y &&
           arg1.x <= this.x + this.width &&
           arg1.y <= this.y + this.height;
};


/* ======================| Parametric functions |====================== */


/**
 * Calculates one point in a one-dimensional quadratic Bezier curve.
 *
 * @param {number} p0 - First control point (start point).
 * @param {number} p1 - Second control point (determines curvature).
 * @param {number} p2 - Third control point (end point).
 * @param {number} t - Time, where `0 <= t <= 1`.
 * @return {number}
 */
function bezier2(p0, p1, p2, t) {
    "use strict";
    const timeComplement = 1 - t;
    return timeComplement * (timeComplement * p0 + t * p1) +
           t              * (timeComplement * p1 + t * p2);
}


/* =====================| Data handling functions |===================== */


/**
 * Takes in an `ArrayBuffer` and returns a `string` that is the result of
 * interpreting the `ArrayBuffer`'s data as ASCII encoded text.
 *
 * @param {ArrayBuffer} buffer
 * @return {string}
 */
function bufferToAscii(buffer) {
    "use strict";
    return u8ArrayToAscii(new Uint8Array(buffer));
}

/**
 * Like `bufferToAscii()`, but takes in a `Uint8Array` for use when the view
 * already exists.
 *
 * @param {Uint8Array} u8arr
 * @return {string}
 */
function u8ArrayToAscii(u8arr) {
    "use strict";
    let s = "";
    for (let i = 0; i < u8arr.length; ++i) {
        s += String.fromCharCode(u8arr[i]);
    }
    return s;
}


/* ========================| Event handling |======================== */


/**
 * Handles deregistering of event listeners.
 */
function EventRegistrar() {
    "use strict";
    this.events = new Map();
}

EventRegistrar.prototype.register = function(target, type, fn) {
    "use strict";
    if (this.events.has(type)) {
        this.events.set(type, this.events.get(type).concat([[target, fn]]));
    } else {
        this.events.set(type, [[target, fn]]);
    }
};

EventRegistrar.prototype.forEach = function(f) {
    "use strict";
    this.events.forEach(
        (pairs, type) => pairs.forEach(
            ([target, fn]) => f(target, type, fn)
        )
    );
};


/* ====================| General purpose functions |==================== */


/**
 * Returns an `Array` of `number`s that represents a "range" between `start`
 * (inclusive) and `end` (exclusive). The range steps by increments of `step`.
 * If `step` is omitted, it defaults to `1`. If, furthermore, `end` is omitted,
 * `end` takes the value of `start`, and `start` takes the value of `0`.
 *
 * If `step` is negative or `end` is less than `start` (or both), the resulting
 * range is descending. Otherwise the range is ascending.
 *
 * @param {number} start - Integer to start at (inclusive).
 * @param {number=} end - Integer to end at (exclusive).
 * @param {number=} step - How much to step by, default 1.
 * @return {number[]} - A range of integers.
 */
function range(start, end, step) {
    "use strict";

    if (end === undefined) {
        end = start;
        start = 0;
    }
    if (!step) {
        step = 1;
    }

    const ret = [];
    if (end < start) {
        step = Math.abs(step);
        for (let i = start; i > end; i -= step) {
            ret.push(i);
        }
    } else if (step < 0) {
        for (let i = end - 1; i >= start; i += step) {
            ret.push(i);
        }
    } else {
        for (let i = start; i < end; i += step) {
            ret.push(i);
        }
    }
    return ret;
}

/**
 * Gets and returns a random element of the passed-in array-like object.
 *
 * By array-like, it is meant that the object supports indexing (`arr[0]`,
 * `arr[1]`, ...) and has a corresponding `.length` property.
 *
 * @template T
 * @param {T[]} arr - The array-like object to choose from.
 * @return {T} - A random element of `arr`.
 */
function getRand(arr) {
    "use strict";
    return arr[Math.floor(arr.length * Math.random())];
}

/**
 * Gets a random integer between `lowerBound` (inclusive) and `upperBound`
 * (exclusive).
 *
 * @param {number} lowerBound
 * @param {number} upperBound
 * @return {number}
 */
function randInt(lowerBound, upperBound) {
    "use strict";
    return Math.floor((upperBound - lowerBound) * Math.random() + lowerBound);
}
