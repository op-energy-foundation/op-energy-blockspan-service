import { Router } from '@angular/router';

export function navigator(router: Router, link: string): void {
  router.navigate([link]);
}

export function toHHMMSS(secs: number): string {
  if (!(secs > 0)) {
    return '??:??:??';
  }

  const hours = Math.floor(secs / 3600);
  const minutes = Math.floor((secs - hours * 3600) / 60);
  const seconds = secs - hours * 3600 - minutes * 60;

  const strHours = hours < 10 ? `0${hours}` : hours.toString();
  const strMinutes = minutes < 10 ? `0${minutes}` : minutes.toString();
  const strSeconds = seconds < 10 ? `0${seconds}` : seconds.toString();

  return `${strHours}:${strMinutes}:${strSeconds}`;
}
