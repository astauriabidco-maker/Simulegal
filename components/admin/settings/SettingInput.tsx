import React, { useState } from 'react';
import { Eye, EyeOff } from 'lucide-react';

export default function SettingInput({ label, value, type = 'text', onChange, isSecret, showSecret, onToggleSecret }: {
    label: string,
    value: string,
    type?: string,
    onChange: (v: string) => void,
    isSecret?: boolean,
    showSecret?: boolean,
    onToggleSecret?: () => void
}) {
    const [localValue, setLocalValue] = useState(value);

    return (
        <div className="space-y-1.5">
            <div className="flex justify-between items-center ml-1">
                <label className="text-[10px] font-black uppercase tracking-widest text-slate-400">{label}</label>
                {isSecret && (
                    <button onClick={onToggleSecret} className="text-slate-400 hover:text-indigo-600 transition-colors">
                        {showSecret ? <EyeOff size={14} /> : <Eye size={14} />}
                    </button>
                )}
            </div>
            <input
                type={isSecret && !showSecret ? 'password' : type}
                value={localValue}
                onChange={e => setLocalValue(e.target.value)}
                onBlur={() => onChange(localValue)}
                className="w-full px-5 py-4 bg-slate-50 border-2 border-slate-100 rounded-2xl font-bold focus:border-indigo-600 focus:bg-white outline-none transition-all placeholder:text-slate-300"
            />
        </div>
    );
}
