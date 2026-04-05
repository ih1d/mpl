import type { CellOutput as CellOutputType } from '../types/notebook';

interface CellOutputProps {
  outputs: CellOutputType[];
}

export function CellOutput({ outputs }: CellOutputProps) {
  if (outputs.length === 0) return null;

  return (
    <div className="cell-outputs">
      {outputs.map((output, i) => (
        <div key={i} className={`cell-output cell-output--${output.type}`}>
          {output.type === 'html' ? (
            <div dangerouslySetInnerHTML={{ __html: output.content }} />
          ) : (
            <pre>{output.content}</pre>
          )}
        </div>
      ))}
    </div>
  );
}
