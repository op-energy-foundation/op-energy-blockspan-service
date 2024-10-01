import { BlockspanBHSComponent } from './../blockspan-bhs/blockspan-bhs.component';
import { ComponentFixture, TestBed } from '@angular/core/testing';

describe('BlockspanBHSComponent', () => {
  let component: BlockspanBHSComponent;
  let fixture: ComponentFixture<BlockspanBHSComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [BlockspanBHSComponent],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(BlockspanBHSComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
