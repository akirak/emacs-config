// This script should be run by deno
type Lock = {
  nodes: NodeMap
  root: string
  version: number
}

type NodeMap = { [key: string]: Node }

type Node = {
  flake?: boolean
  locked?: Source
  original?: Source
  inputs?: { [key: string]: string}
}

type Source = {
  lastModified: Number
  revCount?: number
  narHash: string
  owner?: string
  repo?: string
  url?: string
  path?: string
  rev?: string
  type?: string
  ref?: string
}

async function readFlakeLock(filepath: string): Promise<Lock> {
  return JSON.parse(await Deno.readTextFile(filepath))
}

const { nodes } = await readFlakeLock('emacs/lock/flake.lock')

const entries = Object.entries(nodes)
  .map(x => [x[0], x[1]?.original?.owner])
  .filter(x => !!x[1]) as string[][]

const owners = new Set<string>()

for (const x of entries) {
  owners.add(x[1] as string)
}

const table: [string, number, string][] = []

for (const owner of owners.values()) {
  if (owner === "akirak")
    continue
  const names = entries.filter((x: string[]) => x[1] === owner).map(x => x[0])
  if (names.length > 1) {
    table.push([owner, names.length, names.join(",")])
  }
}

console.table(table.sort((x, y) => y[1] - x[1]))
