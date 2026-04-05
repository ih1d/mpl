import { useNotebook } from '../hooks/useNotebook';
import { Toolbar } from './Toolbar';
import { CodeCell } from './CodeCell';
import { MarkdownCell } from './MarkdownCell';

export function NotebookView() {
  const {
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
  } = useNotebook();

  const lastCellId = notebook.cells[notebook.cells.length - 1]?.id ?? null;

  return (
    <div className="notebook">
      <Toolbar
        title={notebook.metadata.title}
        kernel={notebook.metadata.kernel}
        onExecuteAll={executeAll}
        onClearOutputs={clearAllOutputs}
        onAddCodeCell={() => addCell(lastCellId, 'code')}
        onAddMarkdownCell={() => addCell(lastCellId, 'markdown')}
      />
      <div className="cells">
        {notebook.cells.map((cell) => {
          const common = {
            key: cell.id,
            cell,
            isFocused: focusedCellId === cell.id,
            onFocus: () => setFocusedCellId(cell.id),
            onSourceChange: (source: string) => updateCell(cell.id, { source }),
            onDelete: () => deleteCell(cell.id),
            onMoveUp: () => moveCell(cell.id, 'up'),
            onMoveDown: () => moveCell(cell.id, 'down'),
            onAddCellBelow: () => addCell(cell.id, 'code'),
          };

          if (cell.type === 'code') {
            return (
              <CodeCell
                {...common}
                onExecute={() => executeCell(cell.id)}
                onChangeCellType={() => changeCellType(cell.id, 'markdown')}
              />
            );
          }

          return (
            <MarkdownCell
              {...common}
              onChangeCellType={() => changeCellType(cell.id, 'code')}
            />
          );
        })}
      </div>
      <div className="add-cell-footer">
        <button onClick={() => addCell(lastCellId, 'code')}>+ Code</button>
        <button onClick={() => addCell(lastCellId, 'markdown')}>
          + Markdown
        </button>
      </div>
    </div>
  );
}
