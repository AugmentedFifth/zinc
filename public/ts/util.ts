/* ===========================| Type aliases |=========================== */

type TypedArray = Uint8Array
                | Int8Array
                | Uint8Array
                | Uint8ClampedArray
                | Int16Array
                | Uint16Array
                | Int32Array
                | Uint32Array
                | Float32Array
                | Float64Array;

type TypedArrayConstructor = Uint8ArrayConstructor
                           | Int8ArrayConstructor
                           | Uint8ArrayConstructor
                           | Uint8ClampedArrayConstructor
                           | Int16ArrayConstructor
                           | Uint16ArrayConstructor
                           | Int32ArrayConstructor
                           | Uint32ArrayConstructor
                           | Float32ArrayConstructor
                           | Float64ArrayConstructor;

enum Direction {
    Up,
    Right,
    Down,
    Left
}

type FillStyle = CanvasPattern | string | CanvasGradient;

type Button = [Rect, number, string, (t?: number) => void];

type FormBox = [Rect, string, boolean, number];


/* =============| Extending prototypes of built-in objects |============= */

/**
 * Custom implementation of `Array.prototype.toString()`.
 */
Array.prototype.toString = function(): string {
    return `[${this.join(", ")}]`;
};


interface Set<T> {
    foldl<S>(fold: (accu: S, x: T) => S, seed: S): S;
}

/**
 * Custom left-fold implementation for native ECMAScript `Set`.
 */
Set.prototype.foldl = function<T, S>(fold: (accu: S, x: T) => S, seed: S): S {
    this.forEach(v => seed = fold(seed, v));
    return seed;
};


interface Map<K, V> {
    clearKeysUpTo(n: K): Map<K, V>;
    filter(
        f: (val: V, key: K, _this: Map<K, V>) => boolean
    ): Map<K, V>;
    adjust(
        toAdjust: K, f: (val: V, key: K, _this: Map<K, V>) => V
    ): Map<K, V>;
}

/**
 * Clears the keys in a `Map` that are equal to or less than the specified key.
 *
 * Operates **in place** and returns a reference to `this`.
 */
Map.prototype.clearKeysUpTo = function<K, V>(n: K): Map<K, V> {
    this.forEach((v, k) => {
        if (k <= n) {
            this.delete(k);
        }
    });
    return this;
};

/**
 * Filters elements of this `Map` **in place**.
 *
 * Predicate takes up to 3 params:
 *
 * 1. The value.
 * 2. The key.
 * 3. `this`, the `Map` object being filtered through.
 *
 * Returns `this`, **not** a new `Map`.
 */
Map.prototype.filter = function<K, V>(
    f: (val: V, key: K, _this: Map<K, V>) => boolean
): Map<K, V> {
    this.forEach((v, k, this_) => {
        if (!f(v, k, this_)) {
            this.delete(k);
        }
    });
    return this;
};

/**
 * Adjust the value at the given key in the `Map` using a function to compute
 * the new value.
 */
Map.prototype.adjust = function<K, V>(
    toAdjust: K,
    f: (val: V, key: K, _this: Map<K, V>) => V
): Map<K, V> {
    this.set(toAdjust, f(this.get(toAdjust), toAdjust, this));
    return this;
};


/* ================| Circular buffer implementation |================ */


class CircularBuffer<T> {
    /**
     * Creates a new `CircularBuffer` with the specified `capacity`,
     * which must strictly be a positive integer.
     *
     * We here use the term "zero element", which refers to exactly `null` if
     * the `CircularBuffer` is backed by an `Array`, and refers to exactly `0`
     * if it is backed by a `TypedArray`. Note that using this constructor
     * will always yield a `CircularBuffer` backed by an `Array`. Use the
     * `CircularBuffer.fromArray()` function any time a `TypedArray` backing
     * is required.
     *
     * The `head` and `tail` both start at `0`. The `inhabited` property is a
     * count of how many non-zero elements there are in the buffer.
     */
    public constructor(public capacity: number) {
        if (capacity < 1 || capacity % 1 !== 0) {
            throw new Error(
                "CircularBuffer: capacity must be a positive integer. " +
                    `Got: ${capacity}`
            );
        }
    }

    /** **_Internal use only._** */
    public _buffer: (T | null)[] | TypedArray =
        Array(this.capacity).fill(null);

    public head: number = 0;
    public tail: number = 0;
    public inhabited: number = 0;
    public isArray: boolean = true;

    /**
     * Creates a new `CircularBuffer` from the given `Array` or `TypedArray`.
     * The array cannot be empty. Note that `TypedArray`s will always have an
     * `inhabited` property equal to the number of values that they hold that
     * aren't `0`, since `TypedArray`s store primitives and thus cannot be
     * `null`ed.
     */
    public static fromArray<T>(
        array: (T | null)[] | TypedArray
    ): CircularBuffer<T> {
        const isArray = Array.isArray(array);
        if (isArray && array.length < 1) {
            throw new Error(
                "CircularBuffer.fromArray: array cannot be empty."
            );
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
        const cb: CircularBuffer<T> = new CircularBuffer(array.length);
        cb._buffer = array;
        const zeroElement = isArray ? null : 0;
        for (let i = 0; i < array.length; ++i) {
            if (array[i] !== zeroElement) {
                cb.inhabited++;
            }
        }
        cb.isArray = isArray;
        return cb;
    }

    /**
     * Returns a `string` representation of the `CircularBuffer`, in the form
     * "[object CircularBuffer(`capacity`) capacity `capacity` head `head` tail
     * `tail` inhabited `inhabited` isArray `isArray`]"
     */
    public toString(): string {
        return `[object CircularBuffer(${this.capacity}) capacity ` +
               `${this.capacity} head ${this.head} tail ${this.tail} ` +
               `inhabited ${this.inhabited} isArray ${this.isArray}]`;
    }

    /**
     * "Cons"es the given element at the `head` of the `CircularBuffer`, thus
     * placing this new element in the old `head`'s position and moving the
     * `head` cursor up by one. The old value at the overwritten position is
     * returned (the zero element if it was uninhabited).
     *
     * If the old `head` was overlapping the `tail` and that spot was
     * inhabited, then the `tail` is shifted forward by one as well.
     */
    public cons(val: T | null): T | null;
    public cons(val: number): number;
    public cons(val: T | null | number): T | null | number {
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
    }

    /**
     * Alias for the `CircularBuffer.cons()` method.
     */
    // tslint:disable-next-line:typedef
    public push = this.cons;

    /**
     * Acts as the inverse of `CircularBuffer.cons()`, removing and returning
     * the element at the `head` of the buffer (minus one), pushing the `head`
     * cursor back by 1.
     *
     * **Note** that this will function even if the "element" being unconsed is
     * actually uninhabited (i.e. it is the zero element).
     */
    public uncons(): T | null | number {
        this._decrement_head();
        const oldVal = this._buffer[this.head];
        const zeroElement = this.isArray ? null : 0;
        if (oldVal !== zeroElement) {
            this.inhabited--;
        }
        this._buffer[this.head] = zeroElement;
        return oldVal;
    }

    /**
     * Alias for the `CircularBuffer.uncons()` method.
     */
    // tslint:disable-next-line:typedef
    public pop = this.cons;

    /**
     * Acts like `CircularBuffer.cons()`, but puts an element onto the `tail`
     * of the buffer instead of the `head`, pushing back `tail` by 1.
     *
     * If `tail` pointed to the same spot as `head` before the "snoc", then
     * `head` is also pushed back by 1.
     */
    public snoc(val: T | null | number): T | null | number {
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
    }

    /**
     * Alias for the `CircularBuffer.snoc()` method.
     */
    // tslint:disable-next-line:typedef
    public shift = this.snoc;

    /**
     * Acts as the inverse of `CircularBuffer.snoc()`, removing and returning
     * the element at the `tail` of the buffer, advancing the `tail` cursor by
     * 1.
     *
     * If `tail` and `head` pointed to the same spot before the "unsnoc", then
     * the `head` cursor is also advanced by 1.
     */
    public unsnoc(): T | null | number {
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
    }

    /**
     * Alias for the `CircularBuffer.unsnoc()` method.
     */
    // tslint:disable-next-line:typedef
    public unshift = this.unsnoc;

    /**
     * Gets the value at a specified index in the `CircularBuffer`'s stack.
     * The index supplied (`i`) must be an integer, and this method
     * **interprets the index relative to the current `head` cursor position**
     * (minus one, actually). This represents the element last inserted, i.e.
     * LIFO behavior.
     *
     * To get an element relative to the `tail` cursor position (FIFO), use
     * `CircularBuffer.peek()`. To get an element via its position in the
     * underlying buffer, use `CircularBuffer.bufferGet()`.
     *
     * `i = 0` is the default value of `i`, and gets the element at the current
     * `head` cursor position minus one (i.e. the last element inserted into
     * the `CircularBuffer`). Positive values of `i` walk from the last
     * inserted element to the second-to-last inserted element, and so on.
     *
     * Negative indices walk backward, and going off the end of the buffer
     * wraps circularly as expected.
     */
    public get(i?: number): T | null;
    public get(i?: number): number;
    public get(i: number = 0): T | null | number {
        if (i % 1 !== 0) {
            throw new Error(
                `CircularBuffer.get(): i must be an integer. Got: ${i}`
            );
        }
        return this._buffer[this._clamp_index(this.head - 1 - i)];
    }

    /**
     * Gets the value at a specified index in the `CircularBuffer`'s queue.
     * The index supplied (`i`) must be an integer, and this method
     * **interprets the index relative to the current `tail` cursor position**.
     * This represents the oldest element inserted, i.e. FIFO behavior.
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
     * Negative indices walk backward, and going off the end of the buffer
     * wraps circularly as expected.
     */
    public peek(i?: number): T | null;
    public peek(i?: number): number;
    public peek(i: number = 0): T | null | number {
        if (i % 1 !== 0) {
            throw new Error(
                `CircularBuffer.peek(): i must be an integer. Got: ${i}`
            );
        }
        return this._buffer[this._clamp_index(this.tail + i)];
    }

    /**
     * Gets the value at a specified index in the `CircularBuffer`'s buffer.
     * The index supplied (`i`) must be an integer, and this method does
     * **NOT** interpret the index relative to any cursor position (`head` or
     * `tail`); for that, use `CircularBuffer.get()` or
     * `CircularBuffer.peek()`, respectively. Instead, this index indexes
     * directly into the underlying buffer array.
     *
     * Negative indices walk backward, and going off the end of the buffer
     * wraps circularly as expected.
     */
    public bufferGet(i: number): T | null | number {
        if (i % 1 !== 0) {
            throw new Error(
                `CircularBuffer.bufferGet(): i must be an integer. Got: ${i}`
            );
        }
        return this._buffer[this._clamp_index(i)];
    }

    /**
     * Behaves like `CircularBuffer.get()`, but sets the value at that position
     * and then returns the old value. Also, `i` is not optional for this
     * method.
     */
    public set(i: number, val: T | null | number): T | null | number {
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
    }

    /**
     * Behaves like `CircularBuffer.bufferGet()`, but sets the value at that
     * position and then returns the old value.
     */
    public bufferSet(i: number, val: T | null | number): T | null | number {
        if (i % 1 !== 0) {
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
    }

    /**
     * Behaves like `CircularBuffer.set()`, but sets the value to the zero
     * element, which represents an uninhabited spot for a `CircularBuffer`.
     */
    public delete(i: number): T | null | number {
        const zeroElement = this.isArray ? null : 0;
        return this.set(i, zeroElement);
    }

    /**
     * Behaves like `CircularBuffer.bufferSet()`, but sets the value to the
     * zero element, which represents an uninhabited spot for a
     * `CircularBuffer`.
     */
    public bufferDelete(i: number): T | null | number {
        const zeroElement = this.isArray ? null : 0;
        return this.bufferSet(i, zeroElement);
    }

    /**
     * Clears the entire underlying buffer of all its values and resets both
     * the `head` and `tail` cursors.
     *
     * This is equivalent to `delete`ing all elements, and setting
     * `head = tail = 0`.
     */
    public clear(): void {
        const zeroElement = this.isArray ? null : 0;
        for (let i = 0; i < this.capacity; ++i) {
            this._buffer[i] = zeroElement;
        }
        this.head = 0;
        this.tail = 0;
        this.inhabited = 0;
    }

    /**
     * Gets a shallow copy of all elements in the buffer from `tail` up to but
     * not including the head element (said element being at `head - 1`).
     *
     * The kind of array that is returned is the same as the kind of the
     * underlying buffer (`this.isArray?`).
     */
    public getTail(): (T | null)[] | TypedArray {
        const tailSize =
            this.tail < this.head ?
                this.head - this.tail - 1 :
                this.capacity - 1 - this.tail + this.head;
        const arr =
            this.isArray ?
                [] :
                // tslint:disable-next-line:no-any
                new (this._buffer.constructor as any)(tailSize);
        let i = this.tail;
        let j = 0;
        for (; i !== this.head - 1; i = (i + 1) % this.capacity) {
            arr[j] = this._buffer[i];
            j++;
        }
        return arr;
    }

    /**
     * Gets a shallow copy of all elements in the buffer from the head element
     * (at `head - 1`) down to but not including `tail`.
     *
     * The kind of array that is returned is the same as the kind of the
     * underlying buffer (`this.isArray?`).
     */
    public getInit(): (T | null)[] | TypedArray {
        const initSize =
            this.tail < this.head ?
                this.head - this.tail - 1 :
                this.capacity - 1 - this.tail + this.head;
        const arr =
            this.isArray ?
                [] :
                // tslint:disable-next-line:no-any
                new (this._buffer.constructor as any)(initSize);
        let i = this.head - 1;
        let j = initSize - 1;
        for (; i !== this.tail; i = i < 1 ? this.capacity - 1 : i - 1) {
            arr[j] = this._buffer[i];
            j--;
        }
        return arr;
    }

    /**
     * Gets a shallow copy of the contents of the buffer, as an `Array`.
     */
    public asArray(): (T | null | number)[] {
        const arr = [];
        for (let i = 0; i < this.capacity; ++i) {
            arr.push(this._buffer[i]);
        }
        return arr;
    }

    /**
     * Gets a shallow copy of the contents of the buffer, as a `TypedArray`.
     *
     * When `this.isArray === true`, the type of `TypedArray` to be returned
     * can be specified as an argument to this method (A constructor, like
     * `Int32Array`); the default is `Float64Array`. Otherwise, the type of the
     * underlying buffer is always what is returned.
     *
     * Any elements that are not of type `number` are coerced to `0`.
     */
    public asTypedArray(
        type: TypedArrayConstructor = Float64Array
    ): TypedArray {
        if (this._buffer instanceof Array) {
            const typedArr = new type(this.capacity);
            for (let i = 0; i < this.capacity; ++i) {
                const elem = this._buffer[i];
                if (typeof elem === "number") {
                    typedArr[i] = elem;
                } else {
                    typedArr[i] = 0;
                }
            }
            return typedArr;
        }
        return this._buffer.slice();
    }

    /**
     * Executes the provided function for each element from the tail to the
     * head.
     */
    public forEachTail(f: (val: T | null) => void): void;
    public forEachTail(f: (val: number)   => void): void;
    // tslint:disable-next-line:no-any
    public forEachTail(f: (val: any)      => void): void {
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
    }

    /**
     * Executes the provided function for each element from the head to the
     * tail.
     */
    public forEachHead(f: (val: T | null | number) => void): void {
        let i = this.head - 1;
        for (; i !== this.tail; i = i === 0 ? this.capacity - 1 : i - 1) {
            f(this._buffer[i]);
        }
    }

    /**
     * Executes the provided function for each element from the beginning to
     * the end of the underlying buffer.
     */
    public forEachBuffer(f: (val: T | null) => void): void;
    public forEachBuffer(f: (val: number)   => void): void;
    // tslint:disable-next-line:no-any
    public forEachBuffer(f: (val: any)      => void): void {
        for (let i = 0; i < this._buffer.length; ++i) {
            f(this._buffer[i]);
        }
    }

    /**
     * Maps every value in the underlying buffer to a new value based on the
     * supplied mapping function. **This method does NOT create a new
     * `CircularBuffer`.** It only a returns a reference to `this` to
     * facilitate method chaining.
     *
     * This method takes a second, optional argument, that determines whether
     * the mapping function operates on uninhabited spots or if it leaves them
     * alone. The default behavior is to leave them alone.
     *
     * This method maps elements in the order they exist in the underlying
     * buffer, so don't rely on the oder respecting `head` and `tail`.
     */
    public map(
        f: (val: T | null) => T | null,
        mapZeroes?: boolean
    ): CircularBuffer<T>;
    public map(
        f: (val: number)   => number,
        mapZeroes?: boolean
    ): CircularBuffer<T>;
    public map(
        // tslint:disable-next-line:no-any
        f: (val: any)      => T | null | number,
        mapZeroes:  boolean = false
    ): CircularBuffer<T> {
        if (mapZeroes) {
            for (let i = 0; i < this._buffer.length; ++i) {
                this._buffer[i] = f(this._buffer[i]);
            }
        } else {
            const zeroElement = this.isArray ? null : 0;
            for (let i = 0; i < this._buffer.length; ++i) {
                const elem = this._buffer[i];
                if (elem !== zeroElement) {
                    this._buffer[i] = f(elem);
                }
            }
        }
        return this;
    }

    /**
     * **_Internal use only._**
     */
    private _increment_head(): void {
        this.head = (this.head + 1) % this.capacity;
    }

    /**
     * **_Internal use only._**
     */
    private _increment_tail(): void {
        this.tail = (this.tail + 1) % this.capacity;
    }

    /**
     * **_Internal use only._**
     */
    private _decrement_head(): void {
        this.head = this.head === 0 ? this.capacity - 1 : this.head - 1;
    }

    /**
     * **_Internal use only._**
     */
    private _decrement_tail(): void {
        this.tail = this.tail === 0 ? this.capacity - 1 : this.tail - 1;
    }

    /**
     * **_Internal use only._**
     */
    private _clamp_index(i: number): number {
        const i_ = i % this.capacity;
        if (i_ < 0) {
            return i_ + this.capacity;
        }
        return i_;
    }
}


/* ===================| 2D vector implementation |=================== */


class V2 {
    public constructor(public x: number, public y: number) {}

    /**
     * Error tolerance when checking for equality concerning vectors.
     */
    public static epsilon: number = 1e-12;

    /**
     * Makes a `V2` out of the first two elements of an `Array`.
     */
    public static fromArray(array: number[] | TypedArray): V2 {
        return new V2(array[0], array[1]);
    }

    /**
     * Returns the zero vector.
     */
    public static zero(): V2 {
        return new V2(0, 0);
    }

    /**
     * Returns a unit vector `V2` that is `theta` away from the positive
     * x-axis, anticlockwise.
     *
     * `theta` is in radians.
     */
    public static fromAngle(theta: number): V2 {
        return new V2(Math.cos(theta), Math.sin(theta));
    }

    /**
     * Returns a new `V2` with the given number as both of its entries.
     */
    public static pure(n: number): V2 {
        return new V2(n, n);
    }

    /**
     * Creates a `string` representation of this `V2` and returns it, in the
     * form "V2(`x`, `y`)".
     */
    public toString(): string {
        return `V2(${this.x}, ${this.y})`;
    }

    /**
     * Returns a clone of this vector.
     */
    public clone(): V2 {
        return new V2(this.x, this.y);
    }

    /**
     * Returns an `Array` representation of this vector; an `Array` of
     * length exactly 2.
     */
    public array(): number[] {
        return [this.x, this.y];
    }

    /**
     * Compares two vectors to see if they are the same, within an error of
     * `V2.epsilon`.
     */
    public equals(v: V2): boolean {
        return Math.abs(this.x - v.x) < V2.epsilon &&
               Math.abs(this.y - v.y) < V2.epsilon;
    }

    /**
     * Is this the null vector?
     */
    public null(): boolean {
        return Math.abs(this.x) < V2.epsilon && Math.abs(this.y) < V2.epsilon;
    }

    /**
     * Adds two 2-vectors.
     */
    public add(v: V2): V2 {
        return new V2(this.x + v.x, this.y + v.y);
    }

    /**
     * Subtracts the supplied vector from this vector, returning the new
     * resulting `V2`.
     */
    public sub(v: V2): V2 {
        return new V2(this.x - v.x, this.y - v.y);
    }

    /**
     * Multiplies the supplied scalar by this vector, returning a new `V2` as
     * the result.
     */
    public scalarMult(k: number): V2 {
        return new V2(k * this.x, k * this.y);
    }

    /**
     * Divides this vector by the supplied scalar, returning a new `V2` as the
     * result.
     */
    public scalarDiv(k: number): V2 {
        return new V2(this.x / k, this.y / k);
    }

    /**
     * Dots this vector by the supplied vector, returning a new `V2` as the
     * result.
     */
    public dot(v: V2): number {
        return this.x * v.x + this.y * v.y;
    }

    /**
     * The euclidian norm.
     */
    public norm(): number {
        return Math.sqrt(this.x * this.x + this.y * this.y);
    }

    /**
     * The quadrance.
     */
    public quadrance(): number {
        return this.x * this.x + this.y * this.y;
    }

    /**
     * Returns a new `V2` representing the closest perpendicular vector (with
     * the same norm) to this one, going anticlockwise.
     */
    public perp(): V2 {
        return new V2(-this.y, this.x);
    }

    /**
     * The z-component of the cross product of this vector with the supplied
     * vector in the xy-plane.
     */
    public crossZ(v: V2): number {
        return this.x * v.y - this.y * v.x;
    }

    /**
     * The distance between vectors in metric space.
     */
    public dist(v: V2): number {
        return this.sub(v).norm();
    }

    /**
     * Gets the unit vector that is pointing in the same direction as this one.
     * Acts like `.clone()` for null vectors and unit vectors.
     */
    public normalize(): V2 {
        const quad = this.quadrance();
        if (Math.abs(quad) <= V2.epsilon || Math.abs(quad - 1) <= V2.epsilon) {
            return this.clone();
        }
        const norm = this.norm();
        return new V2(this.x / norm, this.y / norm);
    }

    /**
     * Gets the projection of a vector onto this one.
     */
    public project(v: V2): V2 {
        return this.scalarMult(this.dot(v) / this.quadrance());
    }

    /**
     * Gets the angle of this vector from the positive x-axis, anticlockwise.
     *
     * Result is in radians and is always between `-pi` and `pi`.
     */
    public angle(): number {
        return Math.acos(this.x / this.norm()) * Math.sign(this.y);
    }

    /**
     * Maps a function over both elements of this vector, returning a new `V2`.
     */
    public map(f: (component: number) => number): V2 {
        return new V2(f(this.x), f(this.y));
    }
}

/**
 * Convenience method for making a new vector.
 */
function v2(x: number, y: number): V2 {
    return new V2(x, y);
}


/* ====================| 2D rectangle implementation |==================== */


class Rect {
    public x: number;
    public y: number;

    public width: number;
    public height: number;

    /**
     * Creates a new `Rect` with the specified top-left corner, width, and
     * height. These quantities can be specified in three different ways:
     *
     * - Four arguments, all `numbers`, specifying the x and y values of the
     *   top-left corner and width/height, respectively.
     * - Two arguments, both `V2`s.
     * - One argument, a single `Rect` to be cloned.
     */
    public constructor(
        arg1:  number | V2 | Rect,
        arg2?: number | V2,
        arg3?: number,
        arg4?: number
    ) {
        if (
            typeof arg1 === "number" &&
            typeof arg2 === "number" &&
            typeof arg3 === "number" &&
            typeof arg4 === "number"
        ) {
            this.x = arg1;
            this.y = arg2;
            this.width = arg3;
            this.height = arg4;
        } else if (arg1 instanceof V2 && arg2 instanceof V2) {
            this.x = arg1.x;
            this.y = arg1.y;
            this.width = arg2.x;
            this.height = arg2.y;
        } else if (arg1 instanceof Rect) {
            this.x = arg1.x;
            this.y = arg1.y;
            this.width = arg1.width;
            this.height = arg1.height;
        } else {
            throw "Invalid arguments to `Rect` constructor";
        }
    }

    /**
     * Returns a `string` representation of this `Rect`, in the form
     * "Rect(`x`, `y`, `width`, `height`)".
     */
    public toString(): string {
        return `Rect(${this.x}, ${this.y}, ${this.width}, ${this.height})`;
    }

    /**
     * Decides if this rectangle contains the given point within it.
     *
     * The point can be specified as two separate `number`s or just a single
     * `V2`.
     */
    public contains(arg1: number | V2, arg2?: number): boolean {
        if (typeof arg1 === "number" && arg2 !== undefined) {
            return arg1 >= this.x &&
                   arg2 >= this.y &&
                   arg1 <= this.x + this.width &&
                   arg2 <= this.y + this.height;
        } else if (arg1 instanceof V2) {
            return arg1.x >= this.x &&
                   arg1.y >= this.y &&
                   arg1.x <= this.x + this.width &&
                   arg1.y <= this.y + this.height;
        }
        throw new TypeError("Rect.contains() takes in a V2, or two numbers.");
    }
}

/**
 * Convenience function for `new Rect()`.
 */
function rect(
    arg1:  number | V2 | Rect,
    arg2?: number | V2,
    arg3?: number,
    arg4?: number
): Rect {
    return new Rect(arg1, arg2, arg3, arg4);
}


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
function bezier2(p0: number, p1: number, p2: number, t: number): number {
    const timeComplement = 1 - t;
    return timeComplement * (timeComplement * p0 + t * p1) +
           t              * (timeComplement * p1 + t * p2);
}

function squareAtAngle(c: V2, s: number, theta: number): V2 {
    if (theta >= 7 * Math.PI / 4 || theta < Math.PI / 4) {
        // Right
        const x = c.x + s / 2;
        const y = c.y + s * Math.tan(theta) / 2;
        return v2(x, y);
    }
    if (theta >= Math.PI / 4 && theta < 3 * Math.PI / 4) {
        // Top
        const x = c.x + s * Math.tan(Math.PI / 2 - theta) / 2;
        const y = c.y + s / 2;
        return v2(x, y);
    }
    if (theta >= 3 * Math.PI / 4 && theta < 5 * Math.PI / 4) {
        // Left
        const x = c.x - s / 2;
        const y = c.y + s * Math.tan(Math.PI - theta) / 2;
        return v2(x, y);
    }
    { // Block here so that `x` and `y` aren't shadowed
        // Bottom
        const x = c.x - s * Math.tan(3 * Math.PI / 2 - theta) / 2;
        const y = c.y - s / 2;
        return v2(x, y);
    }
}


/* =====================| Data handling functions |===================== */


/**
 * Checks if the machine this is running on is big endian. If this returns
 * `false`, we can assume little endian.
 */
function isBigEndian(): boolean {
    const buf = new ArrayBuffer(2);
    const u8Arr = new Uint8Array(buf);
    const u16Arr = new Uint16Array(buf);
    u8Arr[0] = 0xAA;
    u8Arr[1] = 0xBB;
    return u16Arr[0] === 0xAABB;
}

/**
 * Wraps byte-level operations so that they can be done irrespective of the
 * client machine's endianness, implicitly converting all results of such
 * operations to little endian (as the majority of clients will be little
 * endian).
 */
class Data {
    public isBigEndian: boolean;

    public constructor() {
        this.isBigEndian = isBigEndian();
    }

    /**
     * Takes in an `ArrayBuffer` and returns a `string` that is the result of
     * interpreting the `ArrayBuffer`'s data as ASCII encoded text.
     */
    public bufferToAscii(buffer: ArrayBuffer): string {
        return this.u8ArrayToAscii(new Uint8Array(buffer));
    }

    /**
     * Like `bufferToAscii()`, but takes in a `Uint8Array` for use when the
     * view already exists.
     */
    // tslint:disable-next-line:prefer-function-over-method
    public u8ArrayToAscii(u8Arr: Uint8Array): string {
        let s = "";
        for (let i = 0; i < u8Arr.length; ++i) {
            s += String.fromCharCode(u8Arr[i]);
        }
        return s;
    }

    /**
     * Turns a 32-bit integer into an array (`Uint8Array`) of bytes.
     */
    // tslint:disable-next-line:prefer-function-over-method
    public i32ToBytes(n: number): Uint8Array {
        const bytes = new Uint8Array(4);
        for (let i = 0; i < 4; ++i) {
            bytes[i] = n & 0xFF;
            n >>= 8;
        }
        return bytes;
    }

    /**
     * Turns a 64-bit floating point number into an array (`Uint8Array`) of
     * bytes.
     */
    public f64ToBytes(n: number): Uint8Array {
        const buf = new ArrayBuffer(8);
        const arr = new Float64Array(buf);
        arr[0] = n;
        if (this.isBigEndian) {
            return new Uint8Array(buf).reverse();
        }
        return new Uint8Array(buf);
    }

    /**
     * Turns a hexadecimal `string` representation of an RGB color (e.g.
     * `#e5b90a`) into a byte array.
     */
    // tslint:disable-next-line:prefer-function-over-method
    public hexRgbToBytes(color: string): Uint8Array {
        const arr = new Uint8Array(3);
        arr[0] = parseInt(color.slice(1, 3), 16);
        arr[1] = parseInt(color.slice(3, 5), 16);
        arr[2] = parseInt(color.slice(5),    16);
        return arr;
    }
}


/* ========================| Event handling |======================== */


/**
 * Handles deregistering of event listeners.
 */
class EventRegistrar {
    public events: Map<string, [EventTarget, EventListener][]>;

    public constructor() {
        this.events = new Map();
    }

    public register(
        target: EventTarget,
        type: string,
        fn: EventListener
    ): void {
        const registeredType = this.events.get(type);
        if (registeredType !== undefined) {
            this.events.set(type, registeredType.concat([[target, fn]]));
        } else {
            this.events.set(type, [[target, fn]]);
        }
    }

    public forEach(
        f: (target: EventTarget, type: string, fn: EventListener) => void
    ): void {
        this.events.forEach(
            (pairs, type) => pairs.forEach(
                ([target, fn]) => f(target, type, fn)
            )
        );
    }
}


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
function range(start: number, end?: number, step?: number): number[] {
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
 */
function getRand<T>(arr: ArrayLike<T>): T {
    return arr[Math.floor(arr.length * Math.random())];
}

/**
 * Gets a random integer between `lowerBound` (inclusive) and `upperBound`
 * (exclusive).
 */
function randInt(lowerBound: number, upperBound: number): number {
    return Math.floor((upperBound - lowerBound) * Math.random() + lowerBound);
}
