export enum TextSelectionTypes {
  NONE = 'None',
  CARET = 'Caret',
  RANGE = 'Range',
}

export enum ArrowDirections {
  LEFT = 'ArrowLeft',
  RIGHT = 'ArrowRight',
}

export enum BlockTypes {
  STRING = 'string',
  NUMBER = 'number',
}

export enum BlockParts {
  PRE_HASH = 'blocktxs-',
}

export enum Logos {
  BLOCKS = '🕋',
  STRIKES = '🔥',
  TIMES = '🕢',
  SATOSHIS = '💰',
  CONTRACT = '🤝',
}

export const GENESIS_K = Math.pow(2, 32) / 600;

export enum FormatType {
  WIDGET = 'widget',
  LINE = 'line',
}

export const APP_CONFIGURATION = {
  SPAN_SIZE: 14,
};
