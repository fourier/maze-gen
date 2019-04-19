(in-package :maze-gen)

(defun dijkstra-shortest-path (grid start-cell end-cell)
  "Calculate the shortest path from the start-cell to end-cell.
Using Dijkstra's algorithm,  backtracking.
Returs a list of cells with a path"
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
  "Breadth-first search of the calculated distances from
given start-cell.
Return  a hash table of visited cells. The key is the cell, the
value is the distance (number)"
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


(defun dijkstra-longest-path (grid)
  "Computes the longest path in the grid.
Returns 2 values:
1. cons (start, end)
- the start and end cells of the longest path;
2. a hash table of visited cells. The key is the cell, the
value is the distance (number)"
  ;; get the hash-table of distances from the starting point
  ;; (0, 0) 
  (let ((distances (dijkstra-bfs grid (grid-cell grid 0 0)))
        (max-distance 0)
        (max-cell (grid-cell grid 0 0)))
    ;; find the cell which is most remote from the start cell 0 0
    (grid-map grid
              (lambda (c)
                (let ((d (gethash c distances)))
                  (when (> d max-distance)
                    (setf max-distance d
                          max-cell c)))))
    ;; ok it is found, now lets try to find all the distances
    ;; from this cell
    (let ((distances (dijkstra-bfs grid max-cell))
          (max-distance 0)
          (new-max-cell max-cell))
      ;; find the cell which is most remote from the
      ;; already found farthest cell
      (grid-map grid
                (lambda (c)
                  (let ((d (gethash c distances)))
                    (when (> d max-distance)
                      (setf max-distance d
                            new-max-cell c)))))
      ;; finally return found values
      (values (cons max-cell new-max-cell) distances))))
    
    

 
