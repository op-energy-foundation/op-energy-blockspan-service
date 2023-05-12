import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { BlockspanComponent } from './blockspan.component';

describe('BlockspanComponent', () => {
  let component: BlockspanComponent;
  let fixture: ComponentFixture<BlockspanComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [BlockspanComponent],
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockspanComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
