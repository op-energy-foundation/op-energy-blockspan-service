import { Router } from '@angular/router';

export function navigator(router: Router, link: string): void {
  router.navigate([link]);
}
