package main

// This experiment investigates if it's possible to implement a performant
// ADT in Go or if it's better to just align with interfaces/structs and
// dynamic dispatch as it works in Go.

type TreeDynamic interface {
	Size() int
}

type LeafDynamic struct {
}

type BranchDynamic struct {
	Left  TreeDynamic
	Right TreeDynamic
}

func (_ LeafDynamic) Size() int {
	return 1
}

func (b BranchDynamic) Size() int {
	return 1 + b.Left.Size() + b.Right.Size()
}

type TreeStatic interface{}

type LeafStatic struct {
}

type BranchStatic struct {
	Left  TreeStatic
	Right TreeStatic
}

func SizeLeafStatic(_ LeafStatic) int {
	return 1
}

func SizeBranchStatic(b BranchStatic) int {
	return 1 + SizeTreeStatic(b.Left) + SizeTreeStatic(b.Right)
}

func SizeTreeStatic(t TreeStatic) int {
	switch t.(type) {
	case LeafStatic:
		return SizeLeafStatic(t.(LeafStatic))
	case BranchStatic:
		return SizeBranchStatic(t.(BranchStatic))
	default:
		panic("Unknown constructor")
	}
}

var exampleTreeDynamic TreeDynamic = BranchDynamic{
	BranchDynamic{
		LeafDynamic{},
		LeafDynamic{},
	},
	BranchDynamic{
		LeafDynamic{},
		LeafDynamic{},
	},
}

var exampleTreeStatic TreeStatic = BranchStatic{
	BranchStatic{
		LeafStatic{},
		LeafStatic{},
	},
	BranchStatic{
		LeafStatic{},
		LeafStatic{},
	},
}
