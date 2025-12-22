package main

import (
    "fmt"
    "log"
    "os"
    "sort"
    "strings"
    "strconv"
)


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

type treeNode struct {
    item vec3
    size int
    parent *treeNode
}

func (n *treeNode) find() *treeNode {
    for n.parent != nil {
        n = n.parent
    }
    return n
}

func union(a *treeNode, b *treeNode) {
    a = a.find()
    b = b.find()
    if a == b {
        return
    }
    if a.size < b.size {
        a.parent = b
        b.size += a.size
    } else {
        b.parent = a
        a.size += b.size
    }
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

    nodes := make(map[vec3]*treeNode)
    for _, u := range points {
        nodes[u] = &treeNode{item: u, size: 1}
    }
    for i, e := range edges {
        union(nodes[e.u], nodes[e.v])
        if nodes[e.u].find().size == len(points) {
            fmt.Printf("got an answer after %d steps!\n", i)
            fmt.Printf("part 2 solution: %d\n", e.u[0] * e.v[0])
            break
        }
    }
}