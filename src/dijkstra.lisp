(in-package :maze-gen)

(defun dijkstra-shortest-path (grid start-cell end-cell)
  (loop with table = (dijkstra-bfs grid start-cell)
        with current-cell = end-cell
        for links = (cell-links current-cell)
        until (eql current-cell start-cell)
        do
        (setf current-cell
              ;; find neighbor with shortest distance,
              ;; just findind minimum in the list
              (loop with d = (gethash (car links) table)
                    with r = (car links)
                    for c in links
                    for d1 = (gethash c table)
                    if (< d1 d)
                    do
                    (setf d d1
                          r c)
                    finally (return r)))
        collecting current-cell into result
        finally (return (cons end-cell result))))
  
(defun dijkstra-bfs (grid start-cell)
  ;; a hash table of visited cells. The key is the cell, the
  ;; value is the distance (number)
  (loop with visited = (make-hash-table)
        ;; initial queue of cells to visit
        with frontier = (make-queue (grid-size grid))
        ;; put the initial cell to the queue with initial distance 0
        ;; the queue elements are pair (cell distance) where
        ;; distance is the distance to the cell
        initially (queue-push frontier (cons start-cell 0))
        ;; continue BFS algorithm until nothing in the queue
        until (queue-emptyp frontier)
        ;; preparation is done, now pickup the next element from
        ;; the queue along with its distance
        for (v . d) = (queue-pop frontier)
        ;; get all cells it is connected to
        for links = (cell-links v)
        do
        ;; mark the picked up cell as visited
        (setf (gethash v visited) d)
        ;; now get the list of all non-visited neighbors and
        ;; push them to the queue with increased distance
        ;; from our current cell
        (incf d)
        (mapc (lambda (c)
                (queue-push frontier (cons c d)))
              (remove-if (lambda (c) (not (null (gethash c visited))))
                         links))
        finally (return visited)))
          
