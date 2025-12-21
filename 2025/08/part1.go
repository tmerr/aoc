package main

import (
    "fmt"
    "log"
    "os"
    "sort"
    "strings"
    "strconv"
)


const MAX_CONNECTIONS = 1000
// value for the example input
// const MAX_CONNECTIONS = 10

type vec3 [3]int

func distSquared(a, b vec3) int {
    d1 := a[0] - b[0]
    d2 := a[1] - b[1]
    d3 := a[2] - b[2]
    return d1*d1 + d2*d2 + d3*d3
}

func parse() []vec3 {
    bs, err := os.ReadFile("input.txt")
    if err != nil {
        log.Fatal(err)
    }
    var res []vec3
    for _, line := range strings.Split(string(bs), "\n") {
        line = strings.ReplaceAll(line, "\r", "")
        if len(line) == 0 {
            continue
        }
        dims := strings.Split(line, ",")
        if len(dims) != 3 {
            log.Fatal(fmt.Errorf("unexpected number of dimensions %d", len(dims)))
        }
        var v vec3
        for i, d := range dims {
            num, err := strconv.Atoi(d)
            if err != nil {
                log.Fatal(err)
            }
            v[i] = num
        }
        res = append(res, v)
    }
    return res
}

type edge struct {
    u vec3
    v vec3
    dist int
}

func main() {
    points := parse()
    var edges []edge
    for i, a := range points {
        for j := 0; j < i; j++ {
            b := points[j]
            edges = append(edges, edge{a, b, distSquared(a, b)})
        }
    }
    sort.Slice(edges, func(i, j int) bool {
        return edges[i].dist < edges[j].dist
    })

    adj := make(map[vec3][]vec3)
    for i, e := range edges {
        if i >= MAX_CONNECTIONS {
            break
        }
        adj[e.u] = append(adj[e.u], e.v)
        adj[e.v] = append(adj[e.v], e.u)
    }

    visited := make(map[vec3]struct{})
    var sizes []int
    for len(visited) < len(adj) {
        // pick an unvisited node.
        var q []vec3
        for u := range adj {
            if _, ok := visited[u]; ok {
                continue
            }
            q = append(q, u)
            visited[u] = struct{}{}
            break
        }
        // BFS and mark everything found as visited.
        var size int
        for len(q) != 0 { 
            head := q[0]
            q = q[1:]
            size++
            for _, v := range adj[head] {
                _, seen := visited[v]
                if !seen {
                    q = append(q, v)
                    visited[v] = struct{}{}
                }
            }
        }
        sizes = append(sizes, size)
    }

    // sort descending
    sort.Slice(sizes, func(i, j int) bool {
        return sizes[i] > sizes[j]
    })

    fmt.Println(sizes)

    answer := 1
    for i, n := range sizes {
        if i >= 3 {
            break
        }
        answer *= n
    }

    fmt.Printf("answer: %d", answer)
}