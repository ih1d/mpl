import { useState, useCallback } from 'react';
import { v4 as uuidv4 } from 'uuid';
import type { Cell, CellType, Notebook } from '../types/notebook';

function createCell(type: CellType, source = ''): Cell {
  return {
    id: uuidv4(),
    type,
    source,
    outputs: [],
    status: 'idle',
    executionCount: null,
  };
}

function createNotebook(): Notebook {
  return {
    cells: [createCell('code')],
    metadata: {
      title: 'Untitled Notebook',
      kernel: 'mpl',
      created: new Date().toISOString(),
      modified: new Date().toISOString(),
    },
  };
}

let globalExecutionCount = 0;

export function useNotebook() {
  const [notebook, setNotebook] = useState<Notebook>(createNotebook);
  const [focusedCellId, setFocusedCellId] = useState<string | null>(null);

  const updateCell = useCallback((id: string, updates: Partial<Cell>) => {
    setNotebook((prev) => ({
      ...prev,
      cells: prev.cells.map((cell) =>
        cell.id === id ? { ...cell, ...updates } : cell
      ),
      metadata: { ...prev.metadata, modified: new Date().toISOString() },
    }));
  }, []);

  const addCell = useCallback((afterId: string | null, type: CellType) => {
    const newCell = createCell(type);
    setNotebook((prev) => {
      const idx = afterId
        ? prev.cells.findIndex((c) => c.id === afterId) + 1
        : prev.cells.length;
      const cells = [...prev.cells];
      cells.splice(idx, 0, newCell);
      return {
        ...prev,
        cells,
        metadata: { ...prev.metadata, modified: new Date().toISOString() },
      };
    });
    setFocusedCellId(newCell.id);
    return newCell.id;
  }, []);

  const deleteCell = useCallback(
    (id: string) => {
      setNotebook((prev) => {
        if (prev.cells.length <= 1) return prev;
        const idx = prev.cells.findIndex((c) => c.id === id);
        const cells = prev.cells.filter((c) => c.id !== id);
        if (focusedCellId === id) {
          const nextIdx = Math.min(idx, cells.length - 1);
          setFocusedCellId(cells[nextIdx]?.id ?? null);
        }
        return {
          ...prev,
          cells,
          metadata: { ...prev.metadata, modified: new Date().toISOString() },
        };
      });
    },
    [focusedCellId]
  );

  const moveCell = useCallback((id: string, direction: 'up' | 'down') => {
    setNotebook((prev) => {
      const idx = prev.cells.findIndex((c) => c.id === id);
      const targetIdx = direction === 'up' ? idx - 1 : idx + 1;
      if (targetIdx < 0 || targetIdx >= prev.cells.length) return prev;
      const cells = [...prev.cells];
      [cells[idx], cells[targetIdx]] = [cells[targetIdx], cells[idx]];
      return {
        ...prev,
        cells,
        metadata: { ...prev.metadata, modified: new Date().toISOString() },
      };
    });
  }, []);

  const changeCellType = useCallback((id: string, type: CellType) => {
    setNotebook((prev) => ({
      ...prev,
      cells: prev.cells.map((cell) =>
        cell.id === id
          ? { ...cell, type, outputs: [], status: 'idle', executionCount: null }
          : cell
      ),
    }));
  }, []);

  const executeCell = useCallback(
    async (id: string) => {
      const cell = notebook.cells.find((c) => c.id === id);
      if (!cell || cell.type !== 'code') return;

      globalExecutionCount++;
      const execCount = globalExecutionCount;

      updateCell(id, { status: 'running', outputs: [], executionCount: execCount });

      try {
        const response = await fetch('http://localhost:8080/execute', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ code: cell.source }),
        });

        if (!response.ok) {
          const text = await response.text();
          updateCell(id, {
            status: 'error',
            outputs: [{ type: 'error', content: text || `HTTP ${response.status}` }],
          });
          return;
        }

        const data = await response.json();
        updateCell(id, {
          status: data.error ? 'error' : 'success',
          outputs: [
            {
              type: data.error ? 'error' : 'text',
              content: data.error || data.result || '',
            },
          ],
        });
      } catch {
        updateCell(id, {
          status: 'error',
          outputs: [
            {
              type: 'error',
              content: 'Could not connect to kernel. Is it running on localhost:8080?',
            },
          ],
        });
      }
    },
    [notebook.cells, updateCell]
  );

  const executeAll = useCallback(async () => {
    for (const cell of notebook.cells) {
      if (cell.type === 'code') {
        await executeCell(cell.id);
      }
    }
  }, [notebook.cells, executeCell]);

  const clearAllOutputs = useCallback(() => {
    setNotebook((prev) => ({
      ...prev,
      cells: prev.cells.map((cell) => ({
        ...cell,
        outputs: [],
        status: 'idle',
        executionCount: null,
      })),
    }));
    globalExecutionCount = 0;
  }, []);

  return {
    notebook,
    focusedCellId,
    setFocusedCellId,
    updateCell,
    addCell,
    deleteCell,
    moveCell,
    changeCellType,
    executeCell,
    executeAll,
    clearAllOutputs,
  };
}
