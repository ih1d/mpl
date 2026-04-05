interface ToolbarProps {
  title: string;
  kernel: string;
  onExecuteAll: () => void;
  onClearOutputs: () => void;
  onAddCodeCell: () => void;
  onAddMarkdownCell: () => void;
}

export function Toolbar({
  title,
  kernel,
  onExecuteAll,
  onClearOutputs,
  onAddCodeCell,
  onAddMarkdownCell,
}: ToolbarProps) {
  return (
    <header className="toolbar">
      <div className="toolbar-left">
        <h1 className="notebook-title">{title}</h1>
        <span className="kernel-badge">{kernel}</span>
      </div>
      <div className="toolbar-actions">
        <button onClick={onAddCodeCell} title="Add code cell">
          + Code
        </button>
        <button onClick={onAddMarkdownCell} title="Add markdown cell">
          + Markdown
        </button>
        <div className="toolbar-divider" />
        <button onClick={onExecuteAll} title="Run all cells">
          Run All
        </button>
        <button onClick={onClearOutputs} title="Clear all outputs">
          Clear Outputs
        </button>
      </div>
    </header>
  );
}
