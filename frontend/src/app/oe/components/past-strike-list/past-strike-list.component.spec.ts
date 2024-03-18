import { ComponentFixture, TestBed } from '@angular/core/testing';

import { PastStrikeListComponent } from './past-strike-list.component';

describe('PastStrikeListComponent', () => {
  let component: PastStrikeListComponent;
  let fixture: ComponentFixture<PastStrikeListComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ PastStrikeListComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(PastStrikeListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
