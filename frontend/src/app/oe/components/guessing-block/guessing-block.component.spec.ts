import { ComponentFixture, TestBed } from '@angular/core/testing';

import { GuessingBlockComponent } from './guessing-block.component';

describe('GuessingBlockComponent', () => {
  let component: GuessingBlockComponent;
  let fixture: ComponentFixture<GuessingBlockComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [GuessingBlockComponent],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(GuessingBlockComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
