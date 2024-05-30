/* The routine quicksort was extracted from the GNU C Library qsort.c
   written by Douglas C. Schmidt (schmidt@ics.uci.edu)
   and adapted to guile by adding an extra pointer less
   to quicksort by Roland Orre <orre@nada.kth.se>.

   The reason to do this instead of using the library function qsort
   was to avoid dependency of the ANSI-C extensions for local functions
   and also to avoid obscure pool based solutions.

   This sorting routine is not much more efficient than the stable
   version but doesn't consume extra memory.
 */

#define SWAP(a, b) do { const SCM _tmp = a; a = b; b = _tmp; } while (0)


/* Order using quicksort.  This implementation incorporates four
   optimizations discussed in Sedgewick:

   1. Non-recursive, using an explicit stack of pointer that store the next
   array partition to sort.  To save time, this maximum amount of space
   required to store an array of MAX_SIZE_T is allocated on the stack.
   Assuming a bit width of 32 bits for size_t, this needs only
   32 * sizeof (stack_node) == 128 bytes.  Pretty cheap, actually.

   2. Chose the pivot element using a median-of-three decision tree.  This
   reduces the probability of selecting a bad pivot value and eliminates
   certain extraneous comparisons.

   3. Only quicksorts NR_ELEMS / MAX_THRESH partitions, leaving insertion sort
   to order the MAX_THRESH items within each partition.  This is a big win,
   since insertion sort is faster for small, mostly sorted array segments.

   4. The larger of the two sub-partitions is always pushed onto the
   stack first, with the algorithm then concentrating on the
   smaller partition.  This *guarantees* no more than log (n)
   stack size is needed (actually O(1) in this case)!  */


/* Discontinue quicksort algorithm when partition gets below this size.
 * This particular magic number was chosen to work best on a Sun 4/260. */
#define MAX_THRESH 4


/* Inline stack abstraction:  The stack size for quicksorting at most as many
 * elements as can be given by a value of type size_t is, as described above,
 * log (MAX_SIZE_T), which is the number of bits of size_t.  More accurately,
 * we would only need ceil (log (MAX_SIZE_T / MAX_THRESH)), but this is
 * ignored below. */

#define STACK_SIZE       (8 * sizeof (size_t))  /* assume 8 bit char */
#define PUSH(low, high)  ((void) ((top->lo = (low)), (top->hi = (high)), ++top))
#define	POP(low, high)	 ((void) (--top, (low = top->lo), (high = top->hi)))
#define	STACK_NOT_EMPTY	 (stack < top)

static void
NAME (SCM *const base_ptr, size_t nr_elems, INC_PARAM
      scm_t_trampoline_2 cmp, SCM less)
{
  /* Stack node declarations used to store unfulfilled partition obligations. */
  typedef struct {
    size_t lo;
    size_t hi;
  } stack_node;

  static const char s_buggy_less[] = "buggy less predicate used when sorting";

#define ELT(i) base_ptr[(i)*INC]

  if (nr_elems == 0)
    /* Avoid lossage with unsigned arithmetic below.  */
    return;

  if (nr_elems > MAX_THRESH)
    {
      size_t lo = 0;
      size_t hi = nr_elems-1;

      stack_node stack[STACK_SIZE];
      stack_node *top = stack + 1;

      while (STACK_NOT_EMPTY)
	{
	  size_t left;
	  size_t right;
	  size_t mid = lo + (hi - lo) / 2;
	  SCM pivot;

	  /* Select median value from among LO, MID, and HI. Rearrange
	     LO and HI so the three values are sorted. This lowers the
	     probability of picking a pathological pivot value and
	     skips a comparison for both the left and right. */

	  SCM_TICK;
	
	  if (scm_is_true ((*cmp) (less, ELT(mid), ELT(lo))))
	    SWAP (ELT(mid), ELT(lo));
	  if (scm_is_true ((*cmp) (less, ELT(hi), ELT(mid))))
	    SWAP (ELT(mid), ELT(hi));
	  else
	    goto jump_over;
	  if (scm_is_true ((*cmp) (less, ELT(mid), ELT(lo))))
	    SWAP (ELT(mid), ELT(lo));
	jump_over:;

	  pivot = ELT(mid);
	  left = lo + 1;
	  right = hi - 1;

	  /* Here's the famous ``collapse the walls'' section of quicksort.
	     Gotta like those tight inner loops!  They are the main reason
	     that this algorithm runs much faster than others. */
	  do
	    {
	      while (scm_is_true ((*cmp) (less, ELT(left), pivot)))
		{
		  left += 1;
		  /* The comparison predicate may be buggy */
		  if (left > hi)
		    scm_misc_error (NULL, s_buggy_less, SCM_EOL);
		}

	      while (scm_is_true ((*cmp) (less, pivot, ELT(right))))
		{
		  right -= 1;
		  /* The comparison predicate may be buggy */
		  if (right < lo)
		    scm_misc_error (NULL, s_buggy_less, SCM_EOL);
		}

	      if (left < right)
		{
		  SWAP (ELT(left), ELT(right));
		  left += 1;
		  right -= 1;
		}
	      else if (left == right)
		{
		  left += 1;
		  right -= 1;
		  break;
		}
	    }
	  while (left <= right);

	  /* Set up pointers for next iteration.  First determine whether
	     left and right partitions are below the threshold size.  If so,
	     ignore one or both.  Otherwise, push the larger partition's
	     bounds on the stack and continue sorting the smaller one. */

	  if ((size_t) (right - lo) <= MAX_THRESH)
	    {
	      if ((size_t) (hi - left) <= MAX_THRESH)
		/* Ignore both small partitions. */
		POP (lo, hi);
	      else
		/* Ignore small left partition. */
		lo = left;
	    }
	  else if ((size_t) (hi - left) <= MAX_THRESH)
	    /* Ignore small right partition. */
	    hi = right;
	  else if ((right - lo) > (hi - left))
	    {
	      /* Push larger left partition indices. */
	      PUSH (lo, right);
	      lo = left;
	    }
	  else
	    {
	      /* Push larger right partition indices. */
	      PUSH (left, hi);
	      hi = right;
	    }
	}
    }

  /* Once the BASE_PTR array is partially sorted by quicksort the rest is
     completely sorted using insertion sort, since this is efficient for
     partitions below MAX_THRESH size. BASE_PTR points to the beginning of the
     array to sort, and END idexes the very last element in the array (*not*
     one beyond it!). */

  {
    size_t tmp = 0;
    size_t end = nr_elems-1;
    size_t thresh = min (end, MAX_THRESH);
    size_t run;

    /* Find smallest element in first threshold and place it at the
       array's beginning.  This is the smallest array element,
       and the operation speeds up insertion sort's inner loop. */

    for (run = tmp + 1; run <= thresh; run += 1)
      if (scm_is_true ((*cmp) (less, ELT(run), ELT(tmp))))
	tmp = run;

    if (tmp != 0)
      SWAP (ELT(tmp), ELT(0));

    /* Insertion sort, running from left-hand-side up to right-hand-side.  */

    run = 1;
    while (++run <= end)
      {
	SCM_TICK;

	tmp = run - 1;
	while (scm_is_true ((*cmp) (less, ELT(run), ELT(tmp))))
	  {
	    /* The comparison predicate may be buggy */
	    if (tmp == 0)
	      scm_misc_error (NULL, s_buggy_less, SCM_EOL);

	    tmp -= 1;
	  }

	tmp += 1;
	if (tmp != run)
	  {
            SCM to_insert = ELT(run);
            size_t hi, lo;

            for (hi = lo = run; --lo >= tmp; hi = lo)
              ELT(hi) = ELT(lo);
            ELT(hi) = to_insert;
	  }
      }
  }
}

#undef SWAP
#undef MAX_THRESH
#undef STACK_SIZE
#undef PUSH
#undef POP
#undef STACK_NOT_EMPTY
#undef ELT

#undef NAME
#undef INC_PARAM
#undef INC

