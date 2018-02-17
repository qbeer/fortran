    program bfs
    implicit none

    integer numberOfVertices
    integer, dimension(:,:), allocatable :: adjecencyList
    integer, dimension(:,:), allocatable :: bfsRoute

    integer p,q
    
    allocate( adjecencyList(numberOfVertices, numberOfVertices) )
    allocate( bfsRoute(numberOfVertices, numberOfVertices) )

    do p = 1, numberOfVertices
        do q = 1, numberOfVertices
            adjecencyList(p,q) = -1
        end do
    end do

    call addEdge(numberOfVertices, 1, 2, adjecencyList)
    call addEdge(numberOfVertices, 2, 3, adjecencyList)
    call addEdge(numberOfVertices, 3, 4, adjecencyList)

    deallocate(adjecencyList)
    deallocate(bfsRoute)

    end

    subroutine addEdge(numberOfVertices, fromVertex, toVertex, adjecencyList)

    integer adjecencyList(numberOfVertices, numberOfVertices)
    integer fromVertex, toVertex
    integer i
    
    i = 1
    do while (adjecencyList(fromVertex, i) .ne. -1 .and. i < numberOfVertices)
        i = i + 1
    end do
    adjecencyList(fromVertex, i) = toVertex
    
    i = 1
    do while (adjecencyList(toVertex, i) .ne. -1 .and. i < numberOfVertices)
        i = i + 1
    end do
    adjecencyList(toVertex, i) = fromVertex

    end subroutine addEdge


    subroutine createBFS(numberOfVertices, bfsRoute)

    integer bfsRoute(numberOfVertices, numberOfVertices)

    logical visitedVertices(numberOfVertices), allVisited

    integer j, originVertex, currentVertex

    integer route(numberOfVertices), queuedVertices(numberOfVertices)

    logical allMinusOne

    integer frontQueued

    do j = 1, numberOfVertices
        visitedVertices(j) = .false.
    end do

    originVertex = 0

    frontQueued = 0

    do while ( allVisited(numberOfVertices, visitedVertices) .neqv.  .true.)
        visitedVertices(originVertex) = .true.
        do j = 1, numberOfVertices
           queuedVertices(j) = -1
           route(j) = -1
        end do

********Push back
        j = 0
        do while (queuedVertices(j) .ne. -1 .and. j < numberOfVertices)
            j = j + 1
        end do
        queuedVertices(j) = originVertex
        
        do while ( allMinusOne .neqv. .false. )
            currentVertex =  queuedVertices(frontQueued)
            frontQueued = frontQueued + 1
************Push back
            j = 0
            do while (route(j) .ne. -1 .and. j < numberOfVertices)
                j = j + 1
            end do
            route(j) = currentVertex
        end do
        
    end do

    end subroutine createBFS

    function allMinusOne(numberOfVertices, queuedVertices)

    integer, intent(in) :: numberOfVertices
    integer, intent(in) :: queuedVertices(numberOfVertices)
    logical :: allMinusOne

    integer i
    integer counter
    counter = 0
    do i = 1, numberOfVertices
        if(queuedVertices(i) .eq. -1) then
            counter = counter + 1
        end if
    end do

    if(counter .eq. numberOfVertices) then
        allMinusOne = .true.
    else
        allMinusOne = .false.
    end if

    end function allMinusOne


    function allVisited(numberOfVertices, visitedVertices)

    integer, intent(in) :: numberOfVertices
    logical, intent(in) :: visitedVertices(numberOfVertices)
    logical :: allVisited

    integer i
    integer counter
    counter = 0
    do i = 1, numberOfVertices
        if(visitedVertices(i) .eqv. .true.) then
            counter = counter + 1
        end if
    end do

    if(counter .eq. numberOfVertices) then
        allVisited = .true.
    else
        allVisited = .false.
    end if

    end function allVisited