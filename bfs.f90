    program bfs
    implicit none

    integer numberOfVertices
    integer, dimension(:,:), allocatable :: adjecencyList
    integer, dimension(:,:), allocatable :: bfsRoute

    integer p,q

    numberOfVertices = 4
    
    allocate( adjecencyList(numberOfVertices, numberOfVertices) )
    allocate( bfsRoute(numberOfVertices, numberOfVertices) )

    do p = 1, numberOfVertices
        do q = 1, numberOfVertices
            adjecencyList(p,q) = -1
            bfsRoute(p,q) = -1
        end do
    end do

    call addEdge(numberOfVertices, 1, 2, adjecencyList)
    call addEdge(numberOfVertices, 2, 3, adjecencyList)
    call addEdge(numberOfVertices, 3, 4, adjecencyList)

    call createBFS(numberOfVertices, bfsRoute, adjecencyList)

    print *, "Deallocating adjecencyList and bfsRoute"

    end program bfs

    subroutine printAdjecencyList(numberOfVertices, adjecencyList)

    integer adjecencyList(numberOfVertices, numberOfVertices)

    integer i, j

    do i = 1, numberOfVertices
        do j = 1, numberOfVertices
            if( adjecencyList(i,j) .ne. -1 ) then
                print *, adjecencyList(i, j)
            end if
        end do
    end do

    end subroutine printAdjecencyList

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



    subroutine createBFS(numberOfVertices, bfsRoute, adjecencyList)

    integer adjecencyList(numberOfVertices, numberOfVertices)

    integer bfsRoute(numberOfVertices, numberOfVertices)

    logical visitedVertices(numberOfVertices), allVisited

    integer j, jj, originVertex, currentVertex, currentBFSroute

    integer route(numberOfVertices), queuedVertices(numberOfVertices)

    integer frontQueued

    do j = 1, numberOfVertices
        visitedVertices(j) = .false.
    end do

    currentBFSroute = 0

    originVertex = 1

    frontQueued = 1

    do while ( allVisited(numberOfVertices, visitedVertices) .neqv. .true. .and. originVertex .le. numberOfVertices)
        print *, "Entered allVisited loop - numberOfVertices", numberOfVertices
        print *, "Origin vertex - ", originVertex
        visitedVertices(originVertex) = .true.
        do j = 1, numberOfVertices
           queuedVertices(j) = -1
           route(j) = -1
        end do

!       Push back
        j = 0
        do while (queuedVertices(j) .ne. -1 .and. j < numberOfVertices)
            j = j + 1
        end do
        queuedVertices(j) = originVertex
        
        do while (  frontQueued .le. numberOfVertices - 1 )
            print *, "Entered allMinusOne loop, frontQueued", frontQueued
            currentVertex =  queuedVertices(frontQueued)
            frontQueued = frontQueued + 1
!           Push back
            j = 0
            do while (route(j) .ne. -1 .and. j < numberOfVertices)
                j = j + 1
            end do
            route(j) = currentVertex

            j = 0
            do j = 1, numberOfVertices
                if( adjecencyList(currentVertex, j) .ne. -1 ) then
                    print *, "adjecencyList(currentVertex, j)", adjecencyList(currentVertex, j)
                    if( visitedVertices( adjecencyList(currentVertex, j) ) .eqv. .false. ) then
                        visitedVertices( adjecencyList(currentVertex, j) ) = .true.
!                       Push back
                        jj = 0
                        do while (queuedVertices(jj) .ne. -1 .and. jj < numberOfVertices)
                            jj = jj + 1
                        end do
                        queuedVertices(jj) = adjecencyList(currentVertex, j)
                        print *, "Added new element to queuedVertices - ", adjecencyList(currentVertex, j)
                    end if
                end if
            end do
        end do
        print *, "Before adding current route to bfsRoute"
        jj = 0
        do while( route(jj) .ne. -1 .and. jj < numberOfVertices)
            bfsRoute(currentBFSroute, jj) = route(jj)
            jj = jj + 1
        end do
        currentBFSroute = currentBFSroute + 1
        print *, "After adding current route to bfsRoute"

!       Find the new origin vertex
        j = 0
        do while(visitedVertices(j) .neqv. .false. .and. j < numberOfVertices)
            j = j + 1
        end do
        originVertex = j
        print *, "Finsihed main loop, originVertex -", originVertex 
    end do

    print *, "Finished createBFS subroutine"

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
        print *, "All are minus one"
    else
        allMinusOne = .false.
        print *, "Not yet all are minus one"
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