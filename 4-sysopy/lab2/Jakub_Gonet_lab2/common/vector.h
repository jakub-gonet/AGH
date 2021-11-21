#if !defined(VECTOR_H)
#define VECTOR_H

/*

Vector holds size, capacity metadata before start of the vector.
In memory this looks like this:

+-------------+-----------------+------------------/ /-+
| size_t size | size_t capacity | vec_type items  / /  |
+-------------+-----------------+----------------/ /---+
                                ^ pointer handed to user

We rely on (size_t)0 == 0, which is sensible assumption on most systems.

*/

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define HAS_MUL_OVERFLOW(a, b) ((a) != 0 && ((a) * (b)) / (a) != (b))

#define vec_type(type) type *

#define vec__get_capacity(vec) ((vec) != NULL ? ((size_t *)(vec))[-2] : 0)
#define vec_get_size(vec) ((vec) != NULL ? ((size_t *)(vec))[-1] : 0)
#define vec_is_empty(vec) (vec_get_size((vec)) == 0)

#define vec__set_capacity(vec, new_capacity)                             \
  do                                                                     \
  {                                                                      \
    if (vec != NULL)                                                     \
    {                                                                    \
      /* we cast vec to size_t * to correctly reference metadata fields, \
       * regardless of vec actual type */                                \
      ((size_t *)(vec))[-2] = (new_capacity);                            \
    }                                                                    \
  } while (0)

#define vec__set_size(vec, new_size)                                     \
  do                                                                     \
  {                                                                      \
    if (vec != NULL)                                                     \
    {                                                                    \
      /* we cast vec to size_t * to correctly reference metadata fields, \
       * regardless of vec actual type */                                \
      ((size_t *)(vec))[-1] = (new_size);                                \
    }                                                                    \
  } while (0)

#define vec__grow(vec, new_capacity)                                        \
  do                                                                        \
  {                                                                         \
    /* We can't use calloc here, because it assumes that every element has  \
     * same size and we use two metadata values that potentially can have   \
     * different sizeof */                                                  \
    assert(!HAS_MUL_OVERFLOW((new_capacity), sizeof(*(vec))));              \
    const size_t vec_size_with_metadata =                                   \
        (new_capacity) * sizeof(*(vec)) + sizeof(size_t) * 2;               \
    if ((vec) == NULL)                                                      \
    {                                                                       \
      size_t *vec_ptr = malloc(vec_size_with_metadata);                     \
      assert(vec_ptr != NULL);                                              \
      /* Prepare pointer for handing it to user, void cast to a_void_       \
       * -Wincompatible-pointer-types  */                                   \
      (vec) = (void *)(&vec_ptr[2]);                                        \
      vec__set_capacity((vec), (new_capacity));                             \
      vec__set_size((vec), 0);                                              \
    }                                                                       \
    else                                                                    \
    {                                                                       \
      /* Move pointer for realloc */                                        \
      size_t *vec_ptr = &((size_t *)(vec))[-2];                             \
      size_t *vec_ptr_realloced = realloc(vec_ptr, vec_size_with_metadata); \
      assert(vec_ptr_realloced != NULL);                                    \
      /* Prepare pointer for handing it to user */                          \
      (vec) = (void *)(&vec_ptr_realloced[2]);                              \
      vec__set_capacity((vec), (new_capacity));                             \
    }                                                                       \
  } while (0)

#define vec_append(vec, value)                            \
  do                                                      \
  {                                                       \
    const size_t capacity = vec__get_capacity((vec));     \
    const size_t size = vec_get_size((vec));              \
    /* (re)alocate if needed, initially capacity is 0 */  \
    if (capacity <= size)                                 \
    {                                                     \
      assert(!HAS_MUL_OVERFLOW(capacity, 2));             \
      vec__grow((vec), capacity == 0 ? 1 : capacity * 2); \
    }                                                     \
    (vec)[size] = (value);                                \
    vec__set_size((vec), size + 1);                       \
  } while (0)

#define vec_clear(vec)         \
  do                           \
  {                            \
    if ((vec) != NULL)         \
    {                          \
      vec__set_size((vec), 0); \
    }                          \
  } while (0)

#define vec_remove_at(vec, i)                                               \
  do                                                                        \
  {                                                                         \
    if ((vec) != NULL)                                                      \
    {                                                                       \
      const size_t size = vec_get_size((vec));                              \
      if ((i) < size)                                                       \
      {                                                                     \
        memmove(&vec[(i)], &vec[(i) + 1], (size - 1 - (i)) * sizeof(*vec)); \
        vec__set_size((vec), size - 1);                                     \
      }                                                                     \
    }                                                                       \
  } while (0)

#define vec_remove_last(vec)                                                   \
  do                                                                           \
  {                                                                            \
    /* We can't return a value in multiline macro (without making it           \
     * unhygenic) without using compiler's extensions like GCC's ({ macro body \
     * }) syntax. To achieve that we could overwrite passed variable when      \
     * declaring macro like: vec_pop_to(vec, ret_val), but this would prevent  \
     * using it without second argument. You can achieve that with __VA_ARGS__ \
     * magic (up to defined amount of arguments, which isn't problem here) but \
     * it introduces another level of complexity. */                           \
                                                                               \
    /* We could also reduce memory footprint by realloc'ing vector when it's   \
     * size drops down to predefined value (usually 0.25 of capacity). It      \
     * isn't necessary though so it's not implemented for simplicity sake. */  \
    vec__set_size((vec), vec_get_size((vec)) - 1);                             \
  } while (0)

#define vec_free(vec)                           \
  do                                            \
  {                                             \
    if ((vec) != NULL)                          \
    {                                           \
      size_t *vec_ptr = &((size_t *)(vec))[-2]; \
      free(vec_ptr);                            \
    }                                           \
  } while (0)

#endif // VECTOR_H
