package main

import (
	"errors"
	"fmt"
	"os"
	"strings"
)

type Pulse struct {
	sender    Module
	val       bool
	receivers []Module
}

type Module interface {
	Name() string
	Init(inputs, outputs []Module)
	React(Pulse) (Pulse, bool)
}

type FlipFlop struct {
	name    string
	outputs []Module
	state   bool
	inp     bool
}

func (f *FlipFlop) Name() string {
	return f.name
}

func (f *FlipFlop) Init(_, outputs []Module) {
	f.outputs = outputs
}

func (f *FlipFlop) React(p Pulse) (Pulse, bool) {
	if p.val {
		// on high pulse, nothing happens.
		return Pulse{}, false
	}
	f.state = !f.state
	return Pulse{sender: f, val: f.state, receivers: f.outputs}, true
}

type Conjunction struct {
	name         string
	outputs      []Module
	memory       map[Module]bool
	onChangeHook func(senderName string, val bool)
}

func (c *Conjunction) Name() string {
	return c.name
}

func (c *Conjunction) Init(inputs, outputs []Module) {
	c.memory = make(map[Module]bool)
	for _, m := range inputs {
		// initially default to remembering a low pulse
		c.memory[m] = false
	}
	c.outputs = outputs
}

func (c *Conjunction) React(p Pulse) (Pulse, bool) {
	if c.memory[p.sender] != p.val {
		c.memory[p.sender] = p.val
		if c.onChangeHook != nil {
			c.onChangeHook(p.sender.Name(), p.val)
		}
	}
	res := Pulse{sender: c, val: false, receivers: c.outputs}
	for _, v := range c.memory {
		if !v {
			res.val = true
		}
	}
	return res, true
}

type Broadcaster struct {
	outputs []Module
}

func (b *Broadcaster) Name() string {
	return "broadcaster"
}

func (b *Broadcaster) Init(_, outputs []Module) {
	b.outputs = outputs
}

func (b *Broadcaster) React(p Pulse) (Pulse, bool) {
	return Pulse{sender: b, val: p.val, receivers: b.outputs}, true
}

type NoopModule struct {
	name   string
	gotLow bool
}

func (n *NoopModule) Name() string {
	return n.name
}

func (n *NoopModule) Init(_, outputs []Module) {}

func (n *NoopModule) React(p Pulse) (Pulse, bool) {
	if !p.val {
		n.gotLow = true
	}
	return Pulse{}, false
}

func propagate(i int, b *Broadcaster) {
	buttonPulse := Pulse{val: false, receivers: []Module{b}}
	var q []Pulse
	p, ok := b.React(buttonPulse)
	if ok {
		q = append(q, p)
	}
	for len(q) > 0 {
		p, q = q[0], q[1:]
		for _, r := range p.receivers {
			p1, ok := r.React(p)
			if ok {
				q = append(q, p1)
			}
		}
	}
	return
}

func gcd(a, b int) int {
	var greatest int
	for i := 1; i <= min(a, b); i++ {
		if a%i == 0 && b%i == 0 {
			greatest = i
		}
	}
	return greatest
}

func lcm(a, b int) int {
	return (a * b) / gcd(a, b)
}

func testLCM() {
	if lcm(3, 2) != 6 {
		panic("your LCM is bad")
	}
	if lcm(5, 10) != 10 {
		panic("your LCM is bad")
	}
	if lcm(10, 20) != 20 {
		panic("your LCM is bad")
	}
}

func run() error {
	bytez, err := os.ReadFile("input.txt")
	if err != nil {
		return err
	}
	rx := &NoopModule{name: "rx"}
	broadcaster := &Broadcaster{}
	modules := make(map[string]Module)
	modules["rx"] = rx
	moduleToOuts := make(map[string][]string)
	moduleToIns := make(map[string][]string)
	// windows pls.
	contents := strings.ReplaceAll(string(bytez), "\r", "")
	for _, line := range strings.Split(contents, "\n") {
		line = strings.Trim(line, " \n")
		if line == "" {
			continue
		}
		var module string
		var outs []string
		for _, tok := range strings.Split(line, " ") {
			if module == "" {
				module = tok
				continue
			}
			if tok == "->" {
				continue
			}
			tok = strings.TrimSuffix(tok, ",")
			outs = append(outs, tok)
		}
		switch module[0] {
		case '%':
			module = module[1:]
			modules[module] = &FlipFlop{name: module}
		case '&':
			module = module[1:]
			modules[module] = &Conjunction{name: module}
		default:
			if module == "broadcaster" {
				modules[module] = broadcaster
			} else {
				panic("unknown module type " + module)
			}
		}
		moduleToOuts[module] = outs
		for _, o := range outs {
			moduleToIns[o] = append(moduleToIns[o], module)
		}
	}
	for m := range modules {
		var outs []Module
		for _, o := range moduleToOuts[m] {
			m2, ok := modules[o]
			if !ok {
				return fmt.Errorf("no module %q", o)
			}
			outs = append(outs, m2)
		}
		var ins []Module
		for _, i := range moduleToIns[m] {
			m2, ok := modules[i]
			if !ok {
				return fmt.Errorf("no module %q", i)
			}
			ins = append(ins, m2)
		}
		modules[m].Init(ins, outs)
	}
	presses := 0
	// based on inspection in graphviz...
	// "nc" is a conjunction before "rx". it will send a pulse if all its inputs are
	// true. A guess: each of the inputs are cyclic and we need to take the LCM.
	const SAMPLES = 5
	trueTiming := make(map[string][]int)
	trueTimingDelta := make(map[string][]int)
	var finalizedSamples int
	modules["nc"].(*Conjunction).onChangeHook = func(senderName string, val bool) {
		if !val {
			// mainly care about true. seems to quickly flip false right after.
			return
		}
		num := len(trueTiming[senderName])
		if num >= SAMPLES {
			return
		}
		trueTiming[senderName] = append(trueTiming[senderName], presses)
		if num == SAMPLES-1 {
			ts := trueTiming[senderName]
			fmt.Printf("collected times for %q: %#v\n", senderName, ts)
			var ts1 []int
			for i := 0; i < SAMPLES-1; i++ {
				ts1 = append(ts1, ts[i+1]-ts[i])
			}
			fmt.Printf("distance between times for %q: %#v\n", senderName, ts1)
			trueTimingDelta[senderName] = ts1
			finalizedSamples += 1
		}
	}
	const NUM_NC_INPUTS = 4
	for finalizedSamples != NUM_NC_INPUTS {
		presses++
		propagate(presses, broadcaster)
	}
	for name, ts := range trueTimingDelta {
		for _, t := range ts {
			if t != ts[0] {
				return fmt.Errorf("no consistent cycle pattern for %q", name)
			}
		}
		if trueTiming[name][0] != ts[0] {
			return errors.New("first event is different than the cycle period...")
		}
	}
	res := 1
	for _, ts := range trueTiming {
		res = lcm(res, ts[0])
	}
	fmt.Printf("button presses = %d\n", res)
	return nil
}

func main() {
	testLCM()
	if err := run(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
