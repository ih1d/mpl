import { useState } from 'react';
import Markdown from 'react-markdown';
import remarkGfm from 'remark-gfm';
import CodeMirror from '@uiw/react-codemirror';
import { markdown } from '@codemirror/lang-markdown';
import type { Cell } from '../types/notebook';

interface MarkdownCellProps {
  cell: Cell;
  isFocused: boolean;
  onFocus: () => void;
  onSourceChange: (source: string) => void;
  onDelete: () => void;
  onMoveUp: () => void;
  onMoveDown: () => void;
  onChangeCellType: () => void;
  onAddCellBelow: () => void;
}

export function MarkdownCell({
  cell,
  isFocused,
  onFocus,
  onSourceChange,
  onDelete,
  onMoveUp,
  onMoveDown,
  onChangeCellType,
  onAddCellBelow,
}: MarkdownCellProps) {
  const [editing, setEditing] = useState(!cell.source);

  return (
    <div
      className={`cell cell--markdown ${isFocused ? 'cell--focused' : ''}`}
      onClick={onFocus}
    >
      <div className="cell-gutter">
        <span className="cell-type-indicator">MD</span>
      </div>
      <div className="cell-content">
        <div className="cell-toolbar">
          <span className="cell-type-label">Markdown</span>
          <div className="cell-actions">
            <button
              onClick={() => setEditing(!editing)}
              title={editing ? 'Preview' : 'Edit'}
            >
              {editing ? 'Preview' : 'Edit'}
            </button>
            <button onClick={onChangeCellType} title="Convert to code">
              To Code
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
        {editing ? (
          <div className="cell-editor">
            <CodeMirror
              value={cell.source}
              onChange={onSourceChange}
              extensions={[markdown()]}
              theme="light"
              basicSetup={{
                lineNumbers: false,
                foldGutter: false,
                highlightActiveLine: isFocused,
              }}
              onKeyDown={(e) => {
                if (e.key === 'Enter' && e.shiftKey) {
                  e.preventDefault();
                  setEditing(false);
                }
              }}
            />
          </div>
        ) : (
          <div
            className="markdown-preview"
            onDoubleClick={() => setEditing(true)}
          >
            {cell.source ? (
              <Markdown remarkPlugins={[remarkGfm]}>{cell.source}</Markdown>
            ) : (
              <p className="placeholder">Double-click to edit markdown...</p>
            )}
          </div>
        )}
      </div>
    </div>
  );
}
