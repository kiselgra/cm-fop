(push "/home/kai/code/cgen/fop-proto/" asdf:*central-registry*)
;(push "/home/eidaex/work/cgen/fop-proto/" asdf:*central-registry*)

(require :cm-fop)
(use-package :cm-fop)

(define-composition-system 3)

(define-target cpu-target)
(define-target cpu-target-omp cpu-target)
(define-target cuda-target)

(define-feature parallel-fn (name args (i N) &body body))
(define-feature pfn-cpu-loop ((i N) &body body))

(defmacro pragma (&rest rest) 
  `(comment ,(format nil "~{ ~a~}" rest) :prefix "#pragma"))

(define-feature in-namespace (&body body))

(implement in-namespace (cpu-target)     `(namespace 'cpu_singlethreaded ,@body))
(implement in-namespace (cpu-target-omp) `(namespace 'cpu_omp ,@body))
(implement in-namespace (cuda-target)    `(namespace 'gpu_cuda ,@body))

(implement parallel-fn (cpu-target)
  `(in-namespace
    (function ,name ,(append args `((int ,N))) -> void
      (pfn-cpu-loop (',i ',N) ,@body))))

(implement pfn-cpu-loop (cpu-target)
  `(for ((int ,i 0) (< ,i ,N) (+= ,i 1))
     ,@body))

(implement pfn-cpu-loop (cpu-target-omp)
  `(progn 
     (pragma omp parallel)
     ,parent))

(implement parallel-fn (cuda-target)
  (let ((kernel (symbol-append 'kernel- name))
	(args+N (append args `((int ,N))))
	(actual (loop for arg in args
		   append (last (cgen::flatten arg)))))
    `(in-namespace
      (function ,kernel ,args+N -> (__global__ void)
	(decl ((int ,i (+ (* blockIdx blockDim) threadIdx)))
	  (if (>= ,i ,N)
	      (return))
	  ,@body))
      (function ,name ,args+N -> void
	(decl ((int thd 256)
	       (int blk (+ (/ ,N thd) (? (% ,N thd) 1 0))))
	  (funcall ,kernel (blk thd) ,@actual ,N))))))

(with-config (make-config cpu-target-omp)
  (parallel-fn accum ((float *a) (float *b)) (idx N)
    (+= a[idx] b[idx])))

(with-config (make-config cuda-target)
  (parallel-fn accum ((float *a) (float *b)) (idx N)
    (+= a[idx] b[idx])))

(function main () -> int
  (return 0))

