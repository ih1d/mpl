export type CellType = 'code' | 'markdown';

export type CellStatus = 'idle' | 'running' | 'success' | 'error';

export interface CellOutput {
  type: 'text' | 'error' | 'html';
  content: string;
}

export interface Cell {
  id: string;
  type: CellType;
  source: string;
  outputs: CellOutput[];
  status: CellStatus;
  executionCount: number | null;
}

export interface Notebook {
  cells: Cell[];
  metadata: {
    title: string;
    kernel: string;
    created: string;
    modified: string;
  };
}
