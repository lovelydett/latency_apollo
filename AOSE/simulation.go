package main

import (
	"fmt"
	"math/rand"
)

// A 2D point
type Point2D struct {
	x, y float32
}

type Vertex2D struct {
	vx, vy float32
}

func (point *Point2D) Move(velocity Vertex2D, ms float32) *Point2D {
	point.x += velocity.vx * ms
	point.y += velocity.vy * ms
	return point
}

type Vehicle struct {
	location   Point2D  // m
	velocity   Vertex2D // m/s
	accelerate Vertex2D // m/s2
}

func spawn_cars(num int) []Vehicle {
	if num == 0 {
		num = 50
	}
	vehicles := make([]Vehicle, num)
	for i, _ := range vehicles {
		vehicles[i].location.x = rand.Float32()
		vehicles[i].location.y = rand.Float32()
		vehicles[i].velocity.vx = rand.Float32()
		vehicles[i].velocity.vy = rand.Float32()
	}

	return vehicles
}

func main() {
	fmt.Println("Starting AOSE simulation")
}
