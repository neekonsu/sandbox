package main

import (
	"fmt"
	"sync"
	"time"
)

func main() {
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		counter("")
		wg.Done()
	}()
}

func counter(thing string) {
	for i := 0; i < 100; i++ {
		fmt.Println(i, thing)
		time.Sleep(1 * time.Second)
	}
}
