/**! foo(+A, -B) is det.
 * Computes B as A + 1.
 * @arg A Input number
 * @arg B Output number, A + 1
 * @example foo(1, X). % X = 2
 */
/** foo(+A, -B) is det.
 * Computes B as A + 1.
 * @arg A Input number
 * @arg B Output number, A + 1
 * @example 
 * :- foo(1, X). % X = 2
 */
foo(A, B) :- B is A + 1.
