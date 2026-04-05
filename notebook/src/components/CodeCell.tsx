import CodeMirror from '@uiw/react-codemirror';
import { javascript } from '@codemirror/lang-javascript';
import type { Cell } from '../types/notebook';
import { CellOutput } from './CellOutput';

interface CodeCellProps {
  cell: Cell;
  isFocused: boolean;
  onFocus: () => void;
  onSourceChange: (source: string) => void;
  onExecute: () => void;
  onDelete: () => void;
  onMoveUp: () => void;
  onMoveDown: () => void;
  onChangeCellType: () => void;
  onAddCellBelow: () => void;
}

const statusIcon: Record<string, string> = {
  idle: '',
  running: '*',
  success: '\u2713',
  error: '!',
};

export function CodeCell({
  cell,
  isFocused,
  onFocus,
  onSourceChange,
  onExecute,
  onDelete,
  onMoveUp,
  onMoveDown,
  onChangeCellType,
  onAddCellBelow,
}: CodeCellProps) {
  const execLabel =
    cell.executionCount !== null ? `[${cell.executionCount}]` : '[ ]';

  return (
    <div
      className={`cell cell--code ${isFocused ? 'cell--focused' : ''} cell--${cell.status}`}
      onClick={onFocus}
    >
      <div className="cell-gutter">
        <span className="execution-count" title={cell.status}>
          {execLabel}
          {statusIcon[cell.status] && (
            <span className={`status-icon status-icon--${cell.status}`}>
              {statusIcon[cell.status]}
            </span>
          )}
        </span>
      </div>
      <div className="cell-content">
        <div className="cell-toolbar">
          <span className="cell-type-label">Code</span>
          <div className="cell-actions">
            <button onClick={onExecute} title="Run cell (Shift+Enter)">
              Run
            </button>
            <button onClick={onChangeCellType} title="Convert to markdown">
              To MD
            </button>
            <button onClick={onMoveUp} title="Move up">
              &uarr;
            </button>
            <button onClick={onMoveDown} title="Move down">
              &darr;
            </button>
            <button onClick={onAddCellBelow} title="Add cell below">
              +
            </button>
            <button onClick={onDelete} className="btn-danger" title="Delete cell">
              &times;
            </button>
          </div>
        </div>
        <div className="cell-editor">
          <CodeMirror
            value={cell.source}
            onChange={onSourceChange}
            extensions={[javascript()]}
            theme="light"
            basicSetup={{
              lineNumbers: true,
              foldGutter: false,
              highlightActiveLine: isFocused,
            }}
            onKeyDown={(e) => {
              if (e.key === 'Enter' && e.shiftKey) {
                e.preventDefault();
                onExecute();
              }
            }}
          />
        </div>
        <CellOutput outputs={cell.outputs} />
      </div>
    </div>
  );
}
