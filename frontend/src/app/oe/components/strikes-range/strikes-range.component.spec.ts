import { ComponentFixture, TestBed } from '@angular/core/testing';

import { StrikesRangeComponent } from './strikes-range.component';

describe('StrikesRangeComponent', () => {
  let component: StrikesRangeComponent;
  let fixture: ComponentFixture<StrikesRangeComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ StrikesRangeComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(StrikesRangeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
